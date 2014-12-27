#![feature(slicing_syntax)]

use std::io::{File,BufferedReader};
use std::num::SignedInt; // abs method
use std::io::fs;
use std::cmp::max;

fn filesize(size: int) -> String {
  let units = "KMGT";
  let mut left = size.abs() as f64;
  let mut unit = -1i;

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
    format!("{:.1}{}iB", left, units.char_at(unit as uint))
  }
}

fn get_comm_for(pid: uint) -> String {
  let cmdline_path = format!("/proc/{}/cmdline", pid);
  match File::open(&Path::new(&cmdline_path)).read_to_string() {
    // s may be empty for kernel threads
    Ok(s) => s.slice_to(max(s.len(), 1)-1).replace("\0", " ").to_string(),
    Err(_) => String::new(),
  }
}

fn get_swap_for(pid: uint) -> int {
  let smaps_path = format!("/proc/{}/smaps", pid);
  let mut file = BufferedReader::new(File::open(&Path::new(smaps_path)));
  let mut s = 0;

  for l in file.lines() {
    let line = match l {
      Ok(s) => s,
      Err(_) => return 0,
    };
    if line.starts_with("Swap:") {
      s += line.words().nth(1).unwrap().parse().unwrap();
    }
  }
  s * 1024
}

fn get_swap() -> Vec<(uint, int, String)> {
  fs::readdir(&Path::new("/proc")).unwrap().iter().filter_map(
    |d| d.filename_str().unwrap().parse().and_then(|pid|
      match get_swap_for(pid) {
       0 => None,
       swap => Some((pid, swap, get_comm_for(pid))),
      }
    )
  ).collect()
}

fn main() {
  // let format = "{:>5} {:>9} {}";
  // let totalFmt = "Total: {:8}";
  let mut swapinfo = get_swap();
  swapinfo.sort_by(|&(_, a, _), &(_, b, _)| { a.cmp(&b) });

  println!("{:>5} {:>9} {}", "PID", "SWAP", "COMMAND");
  let mut total = 0i;
  for &(pid, swap, ref comm) in swapinfo.iter() {
    total += swap;
    println!("{:>5} {:>9} {}", pid, filesize(swap), comm);
  }
  println!("Total: {:8}", filesize(total));
}

// vim: se sw=2:
