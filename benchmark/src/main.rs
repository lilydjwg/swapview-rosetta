#![feature(macro_rules)]

extern crate toml;

#[deriving(Show)]
struct OptionalBenchmarkItem {
  name: String,
  dir: Option<String>,
  cmd: Option<Vec<String>>,
  time_limit: Option<int>,
  valid_percent: Option<int>,
}

#[deriving(Show)]
struct BenchmarkItem {
  name: String,
  dir: String,
  cmd: Vec<String>,
  time_limit: int,
  valid_percent: int,
}

macro_rules! valid_int {
  ($v:ident as $i:ident for $name:expr, ($min:expr, $max:expr)) => ({
    let v = try!($v.as_integer().ok_or(
      format!("{} should be an integer, but got {}", stringify!($i), $v)))
      as int;
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
  let mut valid_percent = None;
  for (key, value) in item.iter() {
    match key.as_slice() {
      "dir" => dir = value.as_str().map(|x| x.to_string()),
      "cmd" => cmd = {
        let cmd_arr = try!(value.as_slice().ok_or(
                format!("cmd must be an array of strings, but got {}", value)));
        let maybe_arr_str: Vec<_> = cmd_arr.iter().map(|ref x| x.as_str()).collect();
        if maybe_arr_str.iter().any(|x| x.is_none()) {
            return Err(format!("cmd must be an array of strings, but got {}", cmd_arr));
        }
        Some(maybe_arr_str.iter().map(|x| x.unwrap().to_string()).collect())
      },
      "time_limit" => valid_int!(value as time_limit for name,
                                 (Some(-1i), None::<int>)),
      "valid_percent" => valid_int!(value as valid_percent for name,
                                    (Some(-1i), Some(101i))),
      _ => return Err(format!("unknown field: {}", key)),
    };
  }

  Ok(OptionalBenchmarkItem{
    name: name, dir: dir, cmd: cmd,
    time_limit: time_limit,
    valid_percent: valid_percent,
  })
}

fn merge_default(item: OptionalBenchmarkItem, default: &Option<OptionalBenchmarkItem>)
    -> Result<BenchmarkItem,String> {
  let mut maybe_dir = item.dir;
  let mut maybe_cmd = item.cmd;
  let mut maybe_time_limit = item.time_limit;
  let mut maybe_valid_percent = item.valid_percent;

  if default.is_some() {
    let d = default.as_ref().unwrap();
    maybe_dir = maybe_dir.or(d.dir.clone());
    maybe_cmd = maybe_cmd.or(d.cmd.clone());
    maybe_time_limit = maybe_time_limit.or(d.time_limit.clone());
    maybe_valid_percent = maybe_valid_percent.or(d.valid_percent.clone());
  }
  let dir = try!(maybe_dir.ok_or(
    format!("{} don't have required field dir", item.name)));
  let cmd = try!(maybe_cmd.ok_or(
    format!("{} don't have required field cmd", item.name)));
  let time_limit = try!(maybe_time_limit.ok_or(
    format!("{} don't have required field time_limit", item.name)));
  let valid_percent = try!(maybe_valid_percent.ok_or(
    format!("{} don't have required field valid_percent", item.name)));

  Ok(BenchmarkItem{
    name: item.name, dir: dir, cmd: cmd,
    time_limit: time_limit,
    valid_percent: valid_percent,
  })
}

fn parse_config(toml: &str) -> Result<Vec<BenchmarkItem>,String> {
  let mut parser = toml::Parser::new(toml.as_slice());
  let config = try!(parser.parse().ok_or(format!("bad TOML data: {}", parser.errors)));

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

fn main() {
  let stdin = std::io::stdin().read_to_string();
  let toml = match stdin {
    Ok(v) => v,
    Err(e) => panic!("can't read input data: {}", e),
  };
  let items = parse_config(toml.as_slice()).unwrap();
  for item in items.iter() {
    println!("{}", item);
  }
}
