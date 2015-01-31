#![feature(slicing_syntax)]
#![feature(io)]
#![feature(os)]
#![feature(collections)]
#![feature(core)]
#![feature(path)]

extern crate toml;
extern crate time;
extern crate glob;

use std::old_io as io;
use std::old_io::Command;
use std::old_io::process::StdioContainer;
use std::iter::AdditiveIterator;
use std::num::Float;
use std::cmp::Ordering::{Less, Greater};
use glob::Pattern;

#[derive(Debug)]
struct OptionalBenchmarkItem {
  name: String,
  dir: Option<String>,
  cmd: Option<Vec<String>>,
  time_limit: Option<i32>,
  count_limit: Option<i32>,
  valid_percent: Option<i32>,
}

#[derive(Debug)]
struct BenchmarkItem {
  name: String,
  dir: String,
  cmd: Vec<String>,
  time_limit: i32,
  count_limit: i32,
  valid_percent: i32,
}

#[derive(Debug)]
struct BenchmarkResult {
  topavg: u64,
  avg: u64,
  min: u64,
  max: u64,
  mdev: u64,
  count: u32,
}

macro_rules! ns2ms {
  ($ns:expr) => ({
    $ns as f64 / 1_000_000.
  })
}

struct AtDir {
  oldpwd: Path,
}

impl AtDir {
  fn new(dir: &str) -> io::IoResult<AtDir> {
    let oldpwd = try!(std::os::getcwd());
    try!(std::os::change_dir(&Path::new(dir)));
    Ok(AtDir { oldpwd: oldpwd })
  }
}

impl std::ops::Drop for AtDir {
  fn drop(&mut self) {
    std::os::change_dir(&self.oldpwd).unwrap();
  }
}

macro_rules! valid_int {
  ($v:ident as $i:ident for $name:expr, ($min:expr, $max:expr)) => ({
    let v = try!($v.as_integer().ok_or(
      format!("{} should be an integer, but got {}", stringify!($i), $v)))
      as i32;
    if $min.is_some() && v < $min.unwrap() {
      return Err(
        format!("{} for {} should be greater than {}, but got {}",
                stringify!($i), $name, $min.unwrap(), v));
    }
    if $max.is_some() && v > $max.unwrap() {
      return Err(
        format!("{} for {} should be less than {}, but got {}",
                stringify!($i), $name, $max.unwrap(), v));
    }
    $i = Some(v);
  })
}

fn parse_item(name: String, conf: &toml::Value)
  -> Result<OptionalBenchmarkItem,String> {
  let maybe_item = conf.as_table();
  let item = match maybe_item {
    Some(x) => x,
    None => return Err(format!("config {} is not a table", conf)),
  };
  let mut dir = None;
  let mut cmd = None;
  let mut time_limit = None;
  let mut count_limit = None;
  let mut valid_percent = None;
  for (key, value) in item.iter() {
    match key.as_slice() {
      "dir" => dir = value.as_str().map(|x| x.to_string()),
      "cmd" => cmd = {
        let cmd_arr = try!(value.as_slice().ok_or(
                format!("cmd must be an array of strings, but got {}", value)));
        let maybe_arr_str: Vec<_> = cmd_arr.iter().map(|ref x| x.as_str()).collect();
        if maybe_arr_str.iter().any(|x| x.is_none()) {
            return Err(format!("cmd must be an array of strings, but got {:?}", cmd_arr));
        }
        if maybe_arr_str.len() == 0 {
          return Err("cmd must be an non-empty array of strings".to_string());
        }
        Some(maybe_arr_str.iter().map(|x| x.unwrap().to_string()).collect())
      },
      "time_limit" => valid_int!(value as time_limit for name,
                                 (Some(0), None::<i32>)),
      "count_limit" => valid_int!(value as count_limit for name,
                                 (Some(0), None::<i32>)),
      "valid_percent" => valid_int!(value as valid_percent for name,
                                    (Some(-1), Some(101))),
      //TODO: use log
      _ => {
        io::stderr().write_fmt(
        format_args!("warning: unknown field: {}", key)).unwrap();
      },
    };
  }

  Ok(OptionalBenchmarkItem{
    name: name, dir: dir, cmd: cmd,
    time_limit: time_limit,
    count_limit: count_limit,
    valid_percent: valid_percent,
  })
}

fn merge_default(item: OptionalBenchmarkItem, default: &Option<OptionalBenchmarkItem>)
    -> Result<BenchmarkItem,String> {
  let mut maybe_dir = item.dir;
  let mut maybe_cmd = item.cmd;
  let mut maybe_time_limit = item.time_limit;
  let mut maybe_count_limit = item.count_limit;
  let mut maybe_valid_percent = item.valid_percent;

  if default.is_some() {
    let d = default.as_ref().unwrap();
    maybe_dir = maybe_dir.or(d.dir.clone());
    maybe_cmd = maybe_cmd.or(d.cmd.clone());
    maybe_time_limit = maybe_time_limit.or(d.time_limit.clone());
    maybe_count_limit = maybe_count_limit.or(d.count_limit.clone());
    maybe_valid_percent = maybe_valid_percent.or(d.valid_percent.clone());
  }
  let dir = try!(maybe_dir.ok_or(
    format!("{} don't have required field dir", item.name)));
  let cmd = try!(maybe_cmd.ok_or(
    format!("{} don't have required field cmd", item.name)));
  let time_limit = try!(maybe_time_limit.ok_or(
    format!("{} don't have required field time_limit", item.name)));
  let count_limit = try!(maybe_count_limit.ok_or(
    format!("{} don't have required field count_limit", item.name)));
  let valid_percent = try!(maybe_valid_percent.ok_or(
    format!("{} don't have required field valid_percent", item.name)));

  let expanded_dir = dir.replace("$name", item.name.as_slice());
  Ok(BenchmarkItem{
    name: item.name, dir: expanded_dir, cmd: cmd,
    time_limit: time_limit,
    count_limit: count_limit,
    valid_percent: valid_percent,
  })
}

fn parse_config(toml: &str) -> Result<Vec<BenchmarkItem>,String> {
  let mut parser = toml::Parser::new(toml.as_slice());
  let config = try!(parser.parse().ok_or(format!("bad TOML data: {:?}", parser.errors)));

  let item_v = try!(config.get("item").ok_or("no item definitions".to_string()));
  let items = try!(item_v.as_table().ok_or("item definitions should be in a table".to_string()));

  let default_item = match items.get("default") {
    Some(x) => Some(try!(parse_item("default".to_string(), x))),
    None => None,
  };

  let mut ret = Vec::with_capacity(items.len());
  for (name, conf) in items.iter() {
    if name.as_slice() == "default" {
      continue;
    }
    ret.push(
      try!(merge_default(
          try!(parse_item(name.to_string(), conf)), &default_item
        )
      )
    );
  }
  Ok(ret)
}

fn time_item(item: &BenchmarkItem) -> Result<BenchmarkResult,String> {
  let start = time::precise_time_ns();
  let limit = item.time_limit as u64 * 1_000_000_000u64;

  let _cwd = match AtDir::new(item.dir.as_slice()) {
    Ok(atdir) => atdir,
    Err(err) => return Err(err.desc.to_string()),
  };
  let mut result = Vec::new();

  for _ in range(0, item.count_limit) {
    let (used, now) = try!(run_once(&item.cmd));
    result.push(used);
    if now - start > limit {
      break;
    }
  }

  if result.len() == 0 {
    return Err("no result???".to_string());
  }
  result.sort();
  let len = result.len() as u64;
  let min = *result.first().unwrap();
  let max = *result.last().unwrap();
  let sum = result.iter().map(|&x| x).sum();
  let sum2 = result.iter().map(|&x| x*x).sum();
  let avg = sum / len;
  let mdev = ((sum2 / len - avg * avg) as f64).sqrt() as u64;

  let top_n = ((result.len() * item.valid_percent as usize) as f64 / 100.).round() as usize;
  let tops = &result[..top_n];
  let topavg = tops.iter().map(|&x| x).sum() / top_n as u64;
  Ok(BenchmarkResult {
    topavg: topavg,
    avg: avg,
    min: min,
    max: max,
    mdev: mdev,
    count: result.len() as u32,
  })
}

fn run_once(cmd: &Vec<String>) -> Result<(u64,u64),String> {
  let start = time::precise_time_ns();
  let status = match Command::new(cmd[0].as_slice()).args(&cmd[1..])
                     .stdin(StdioContainer::Ignored)
                     .stdout(StdioContainer::Ignored)
                     .stderr(StdioContainer::InheritFd(2))
                     .status() {
    Ok(status) => status,
    Err(err) => return Err(err.desc.to_string()),
  };

  if !status.success() {
    return Err(format!("command {:?} exited with {}", cmd, status));
  }

  let stop = time::precise_time_ns();
  Ok((stop - start, stop))
}

#[allow(unused_must_use)]
fn main() {
  let stdin = io::stdin().read_to_string();
  let toml = match stdin {
    Ok(v) => v,
    Err(e) => panic!("can't read input data: {}", e),
  };
  let items = parse_config(toml.as_slice()).unwrap();

  let args: Vec<_> = std::os::args().tail()
    .iter().map(|x| Pattern::new(x.as_slice()).unwrap()).collect();
  let items_to_run = if args.len() > 0 {
    items.into_iter().filter(
      |x| args.iter().any(|p| p.matches(x.name.as_slice()))).collect()
  } else {
    items
  };

  let mut stderr = io::stderr();
  let mut results: Vec<_> = items_to_run.iter().map(|x| {
    stderr.write_fmt(format_args!("Running {}...", x.name));
    stderr.flush();
    let r = time_item(x);
    stderr.write_fmt(format_args!("{:?}\n", r));
    (x.name.as_slice(), r)
  }).collect();
  results.sort_by(|&(m, ref a), &(n, ref b)|
    match (a.as_ref(), b.as_ref()) {
      (Ok(c), Ok(d)) => c.topavg.cmp(&d.topavg),
      (Err(_), Err(_)) => m.cmp(n),
      (Ok(_), _) => Less,
      _ => Greater,
    }
  );
  for &(name, ref r) in results.iter() {
    match r.as_ref() {
      Ok(x) => {
        println!("{green}{:>24}{r}: top: {bright}{:>7.2}{r}, min: {:>7.2}, \
avg: {:>7.2}, max: {:>7.2}, mdev: {:>7.2}, cnt: {:>3}",
                 name, ns2ms!(x.topavg), ns2ms!(x.min), ns2ms!(x.avg),
                 ns2ms!(x.max), ns2ms!(x.mdev), x.count,
                 green = "\x1b[1;32m", bright = "\x1b[1;37m",
                 r = "\x1b[m",
                 );
      },
      Err(x) => println!("{red}{:>24}{r}: FAILED with {}", name, x,
                         red = "\x1b[1;31m", r = "\x1b[m"),
    };
  }
}
