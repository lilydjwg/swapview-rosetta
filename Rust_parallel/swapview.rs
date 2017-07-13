extern crate threadpool;
extern crate num_cpus;

use std::fs::{File,read_dir};
use std::io::{Read,BufReader,BufRead};
use threadpool::ThreadPool;
use std::sync::mpsc::channel;

const UNITS: [char; 4] = ['K', 'M', 'G', 'T'];

fn filesize(size: isize) -> String {
  let mut left = size.abs() as f64;
  let mut unit = -1;

  while left > 1100. && unit < 3 {
    left /= 1024.;
    unit += 1;
  }
  if unit == -1 {
    format!("{}B", size)
  } else {
    if size < 0 {
      left = -left;
    }
    format!("{:.1}{}iB", left, UNITS[unit as usize])
  }
}

fn chop_null(s: String) -> String {
  let last = s.len() - 1;
  let mut s = s;
  if !s.is_empty() && s.as_bytes()[last] == 0 {
    s.truncate(last);
  }
  s.replace("\0", " ")
}

fn get_comm_for(pid: usize) -> String {
  let cmdline_path = format!("/proc/{}/cmdline", pid);
  let mut buf = String::new();
  let mut file = match File::open(&cmdline_path) {
    Ok(f) => f,
    Err(_) => return String::new(),
  };
  match file.read_to_string(&mut buf) {
    Ok(_) => (),
    Err(_) => return String::new(),
  };
  chop_null(buf)
}

fn get_swap_for(pid: usize) -> isize {
  let mut s = 0;
  let smaps_path = format!("/proc/{}/smaps", pid);
  let file = match File::open(&smaps_path) {
    Ok(f) => f,
    Err(_) => return 0,
  };
  let reader = BufReader::new(file);

  for l in reader.lines() {
    let line = match l {
      Ok(s) => s,
      Err(_) => return 0,
    };
    if line.starts_with("Swap:") {
      s += line.split_whitespace().nth(1).unwrap().parse::<isize>().unwrap();
    }
  }
  s * 1024
}

fn get_swap() -> Vec<(usize, isize, String)> {
  let pool = ThreadPool::new(num_cpus::get());
  let (tx, rx) = channel();
  let mut count = 0;
  for d in read_dir("/proc").unwrap() {
    let path = d.unwrap().path();
    if let Ok(pid) = path.file_name().unwrap().to_str().unwrap().parse() {
      let tx = tx.clone();
      pool.execute(move || {
        tx.send(match get_swap_for(pid) {
          0 => None,
          swap => Some((pid, swap, get_comm_for(pid))),
        }).unwrap();
      });
      count += 1;
    }
  };
  rx.iter().take(count).filter_map(|x| x).collect()
}

fn main() {
  // let format = "{:>5} {:>9} {}";
  // let totalFmt = "Total: {:8}";
  let mut swapinfo = get_swap();
  swapinfo.sort_by(|&(_, a, _), &(_, b, _)| { a.cmp(&b) });

  println!("{:>5} {:>9} {}", "PID", "SWAP", "COMMAND");
  let mut total = 0;
  for &(pid, swap, ref comm) in &swapinfo {
    total += swap;
    println!("{:>5} {:>9} {}", pid, filesize(swap), comm);
  }
  println!("Total: {:>8}", filesize(total));
}

// vim: se sw=2:
