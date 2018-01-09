#!/usr/bin/env crystal

FORMAT = "%5s %9s %s"
TOTALFMT = "Total: %8s"

def filesize(size)
  units = %w(B KiB MiB GiB TiB)
  left = size.abs
  unit = 0
  num = 0
  units.each_with_index do |_, i|
    unit = i
    num = left / 1024.0 ** i
    break if num <= 1100 || i == units.size - 1
  end
  if unit == 0
    "#{size}B"
  else
    "%.1f%s" % [size < 0 ? -num : num, units[unit]]
  end
end

SWAP = "Swap: "
# since strings (created from literal) are mutable, each iteration will create
# a new string instance if we put literal to map!{}
def get_swap_for(pid)
  comm = File.read("/proc/#{pid}/cmdline")
  comm.rchop if comm[-1] == "\0"
  comm.tr("\0", " ")
  result = File.read("/proc/#{pid}/smaps").split('\n')
  result.select! { |l| l.starts_with? SWAP }
  res = result.map { |l| l[6..-1].to_i }
  s = res.reduce { |acc, i| acc + i }
  {pid, s * 1024, comm}
rescue
  {pid, 0, nil}
end

def get_swap
  result = Dir.entries("/proc")
  result.select! {|dir| dir.to_i? }
  res = result.map {|dir| get_swap_for(dir) }
  res.select! {|s| s[1] > 0 }
  res.sort_by! {|x| x[1] }
  res
end

def main
  results = get_swap
  puts FORMAT % %w(PID SWAP COMMAND)
  results.each do |value|
    pid, swap, comm = value
    puts FORMAT % [pid, filesize(swap), comm]
  end
  res = results.map {|x| x[1] }
  if res.empty?
    t = 0
  else
    t = res.reduce { |acc, i| acc + i }
  end
  puts TOTALFMT % filesize(t)
end

main
