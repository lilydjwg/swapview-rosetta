extern crate toml;
extern crate glob;

use std::io;
use std::time;
use std::process::Command;
use std::path::{Path,PathBuf};
use std::process::Stdio;
use std::cmp::Ordering::{Less, Greater};
use std::io::{Read,Write};
use std::error::Error;
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
  oldpwd: PathBuf,
}

impl AtDir {
  fn new(dir: &str) -> io::Result<AtDir> {
    let oldpwd = std::env::current_dir()?;
    std::env::set_current_dir(&Path::new(dir))?;
    Ok(AtDir { oldpwd })
  }
}

impl std::ops::Drop for AtDir {
  fn drop(&mut self) {
    std::env::set_current_dir(&self.oldpwd).unwrap();
  }
}

macro_rules! valid_int {
  ($v:ident as $i:ident for $name:expr, ($min:expr, $max:expr)) => ({
    let v = $v.as_integer().ok_or(
      format!("{} should be an integer, but got {}", stringify!($i), $v))?
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
  for (key, value) in item {
    match key.as_ref() {
      "dir" => dir = value.as_str().map(|x| x.to_string()),
      "cmd" => cmd = {
        let cmd_arr = value.as_array().ok_or_else(||
          format!("cmd must be an array of strings, but got {}", value))?;
        let maybe_arr_str: Vec<_> = cmd_arr.iter().map(|x| x.as_str()).collect();
        if maybe_arr_str.iter().any(|x| x.is_none()) {
          return Err(format!("cmd must be an array of strings, but got {:?}", cmd_arr));
        }
        if maybe_arr_str.is_empty() {
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
      _ => {
        eprintln!("warning: unknown field: {}", key);
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

  if let Some(ref d) = *default {
    maybe_dir = maybe_dir.or_else(|| d.dir.clone());
    maybe_cmd = maybe_cmd.or_else(|| d.cmd.clone());
    maybe_time_limit = maybe_time_limit.or_else(|| d.time_limit);
    maybe_count_limit = maybe_count_limit.or_else(|| d.count_limit);
    maybe_valid_percent = maybe_valid_percent.or_else(|| d.valid_percent);
  }

  let dir;
  let cmd;
  let time_limit;
  let count_limit;
  let valid_percent;
  {
    let name = &item.name;
    dir = maybe_dir.ok_or_else(||
      format!("{} doesn't have required field dir", name))?;
    cmd = maybe_cmd.ok_or_else(||
      format!("{} doesn't have required field cmd", name))?;
    time_limit = maybe_time_limit.ok_or_else(||
      format!("{} doesn't have required field time_limit", name))?;
    count_limit = maybe_count_limit.ok_or_else(||
      format!("{} doesn't have required field count_limit", name))?;
    valid_percent = maybe_valid_percent.ok_or_else(||
      format!("{} doesn't have required field valid_percent", name))?;
  }

  let expanded_dir = dir.replace("$name", &item.name);
  Ok(BenchmarkItem{
    name: item.name, dir: expanded_dir, cmd: cmd,
    time_limit: time_limit,
    count_limit: count_limit,
    valid_percent: valid_percent,
  })
}

fn parse_config(toml: &str) -> Result<Vec<BenchmarkItem>,String> {
  let config: toml::Value  = toml.parse().map_err(|x: toml::de::Error| x.to_string())?;

  let item_v = config.get("item").ok_or_else(|| "no item definitions".to_string())?;
  let items = item_v.as_table().ok_or_else(|| "item definitions should be in a table".to_string())?;

  let default_item = match items.get("default") {
    Some(x) => Some(parse_item("default".to_string(), x)?),
    None => None,
  };

  let mut ret = Vec::with_capacity(items.len());
  for (name, conf) in items {
    if name == "default" {
      continue;
    }
    let item = parse_item(name.to_string(), conf)?;
    ret.push(merge_default(item, &default_item)?);
  }
  Ok(ret)
}

fn time_item(item: &BenchmarkItem) -> Result<BenchmarkResult,String> {
  let start = time::Instant::now();
  let limit = time::Duration::from_secs(item.time_limit as u64);

  let _cwd = match AtDir::new(&item.dir) {
    Ok(atdir) => atdir,
    Err(err) => return Err(Error::description(&err).to_string()),
  };
  let mut result = vec![];

  for _ in 0..item.count_limit {
    let (used, now) = run_once(&item.cmd)?;
    result.push(used);
    if now - start > limit {
      break;
    }
  }

  if result.is_empty() {
    return Err("no result???".to_string());
  }
  result.sort();
  let len = result.len() as u64;
  let min = *result.first().unwrap();
  let max = *result.last().unwrap();
  let sum: u64 = result.iter().fold(0, |acc, x| acc + x);
  let sum2: u64 = result.iter().map(|&x| x*x).fold(0, |acc, x| acc + x);
  let avg = sum / len;
  let mdev = ((sum2 / len - avg * avg) as f64).sqrt() as u64;

  let top_n = ((result.len() * item.valid_percent as usize) as f64 / 100.).round() as usize;
  let tops = &result[..top_n];
  let topavg = tops.iter().fold(0, |acc, x| acc + x) as f64 / top_n as f64;
  Ok(BenchmarkResult {
    topavg: topavg as u64,
    avg: avg,
    min: min,
    max: max,
    mdev: mdev,
    count: result.len() as u32,
  })
}

fn run_once(cmd: &[String]) -> Result<(u64,time::Instant),String> {
  let start = time::Instant::now();
  let status = match Command::new(&cmd[0]).args(&cmd[1..])
                     .stdin(Stdio::null())
                     .stdout(Stdio::null())
                     .stderr(Stdio::inherit())
                     .status() {
    Ok(status) => status,
    Err(err) => return Err(Error::description(&err).to_string()),
  };

  if !status.success() {
    return Err(format!("command {:?} exited with {}", cmd, status));
  }

  let stop = time::Instant::now();
  let used = stop - start;
  Ok((used.as_secs() * 1_000_000_000 + used.subsec_nanos() as u64, stop))
}

fn main() {
  let mut toml = String::new();
  let result = io::stdin().read_to_string(&mut toml);
  if let Err(e) = result {
    panic!("can't read input data: {}", e);
  }
  let items = parse_config(&toml).unwrap();

  let args: Vec<_> = std::env::args().skip(1)
    .map(|x| Pattern::new(&x).unwrap()).collect();
  let items_to_run = if !args.is_empty() {
    items.into_iter().filter(
      |x| args.iter().any(|p| p.matches(&x.name))).collect()
  } else {
    items
  };

  let mut stderr = io::stderr();
  let mut results: Vec<_> = items_to_run.iter().map(|x| {
    write!(stderr, "Running {}...", x.name).unwrap();
    stderr.flush().unwrap();
    let r = time_item(x);
    write!(stderr, "{:?}\n", r).unwrap();
    (&x.name, r)
  }).collect();
  results.sort_by(|&(m, ref a), &(n, ref b)|
    match (a.as_ref(), b.as_ref()) {
      (Ok(c), Ok(d)) => c.topavg.cmp(&d.topavg),
      (Err(_), Err(_)) => m.cmp(n),
      (Ok(_), _) => Less,
      _ => Greater,
    }
  );
  for &(name, ref r) in &results {
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
