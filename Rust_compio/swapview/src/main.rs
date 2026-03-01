use std::fs::{read_dir, DirEntry};
use std::io::Result;

use compio::{fs::File, io::AsyncReadAtExt};

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

async fn get_comm_for(pid: usize) -> String {
  let cmdline_path = format!("/proc/{}/cmdline", pid);
  let Ok(mut file) = File::open(&cmdline_path).await
    else { return String::new() };
  let bufres = file.read_to_string_at(
    String::with_capacity(1024), 0).await;
  if bufres.0.is_ok() {
    chop_null(bufres.1)
  } else {
    String::new()
  }
}

async fn get_swap_for(pid: usize) -> isize {
  let mut s = 0;
  let smaps_path = format!("/proc/{}/smaps", pid);
  let Ok(file) = File::open(&smaps_path).await
    else { return 0 };

  let (_, buffer) = file.read_to_end_at(
    Vec::with_capacity(1024), 0).await.unwrap();
  for line in buffer.split(|&c| c == b'\n') {
    if line.starts_with(b"Swap:") {
      let string = line[5..]
        .iter()
        .skip_while(|&&c| c == b' ')
        .take_while(|&&c| c != b' ')
        .map(|&c| c as char)
        .collect::<String>();
      s += string.parse::<isize>().unwrap();
    }
  }
  s * 1024
}

async fn get_swap() -> Vec<(usize, isize, String)> {
  let futures: Vec<_> = read_dir("/proc").unwrap()
    .map(|d: Result<DirEntry>| async {
      let path = d.unwrap().path();
      let Ok(pid) = path.file_name().unwrap().to_str().unwrap().parse()
        else { return None };
      match get_swap_for(pid).await {
        0 => None,
        swap => Some((pid, swap, get_comm_for(pid).await)),
      }
    }).collect();
  futures::future::join_all(futures).await
    .into_iter().flatten().collect()
}

#[compio::main]
async fn main() {
  let mut swapinfo = get_swap().await;
  swapinfo.sort_unstable_by(|&(_, a, _), &(_, b, _)| { a.cmp(&b) });

  println!("{:>7} {:>9} {}", "PID", "SWAP", "COMMAND");
  let mut total = 0;
  for &(pid, swap, ref comm) in &swapinfo {
    total += swap;
    println!("{:>7} {:>9} {}", pid, filesize(swap), comm);
  }
  println!("Total: {:>10}", filesize(total));
}

// vim: se sw=2:
