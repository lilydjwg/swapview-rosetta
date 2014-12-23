#![feature(phase)]
#![feature(slicing_syntax)]
#[phase(plugin)]
extern crate regex_macros;
extern crate regex;

use std::io::{File,BufferedReader};
use std::num::SignedInt; // abs method
use std::io::fs;

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
    Ok(s) => s.replace("\0", " ").trim_right().into_string(),
    Err(_) => String::new(),
  }
}

fn get_swap_for(pid: uint) -> int {
  let smaps_path = format!("/proc/{}/smaps", pid);
  let mut file = BufferedReader::new(File::open(&Path::new(smaps_path)));
  let mut s = 0;
  let number_re = regex!(r"\d+");

  for l in file.lines() {
    let line = match l {
      Ok(s) => s,
      Err(_) => return 0,
    };
    if line.starts_with("Swap:") {
      let (start, stop) = number_re.find(line.as_slice()).unwrap();
      s += from_str(line[start..stop]).unwrap();
    }
  }
  s * 1024
}

fn get_swap() -> Vec<(uint, int, String)> {
  let mut ret = Vec::new();
  for d in fs::readdir(&Path::new("/proc")).unwrap().iter() {
    let pid: uint = match from_str(d.filename_str().unwrap()) {
      Some(pid) => pid,
      None => continue,
    };
    ret.push((pid, get_swap_for(pid), get_comm_for(pid)));
  }
  ret
}

fn main() {
  // let format = "{:5} {:>9} {}";
  // let totalFmt = "Total: {:8}";
  let mut swapinfo = get_swap();
  swapinfo.sort_by(|&(_, a, _), &(_, b, _)| { a.cmp(&b) });

  println!("{:5} {:>9} {}", "PID", "SWAP", "COMMAND");
  let mut total = 0i;
  for &(pid, swap, ref comm) in swapinfo.iter() {
    total += swap;
    println!("{:5} {:>9} {}", pid, filesize(swap), comm);
  }
  println!("Total: {:8}", filesize(total));
}

// vim: se sw=2:
