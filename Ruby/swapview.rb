#!/usr/bin/env ruby

FORMAT = "%5s %9s %s"
TOTALFMT = "Total: %8s"

def filesize size
  units = %w(B KiB MiB GiB TiB)
  left = size.abs
  num, unit = units.each_with_index do |_, i|
    l = left / 1024.0 ** i
    break l, i if l <= 1100 or i == units.length - 1
  end
  if unit == 0
    "#{size}B"
  else
    '%.1f%s' % [size < 0 ? -num : num, units[unit]]
  end
end

SWAP = 'Swap: '
# since strings (created from literal) are mutable, each iteration will create
# a new string instance if we put literal to map!{}
def get_swap_for pid
  comm = File.read("/proc/#{pid}/cmdline")
  comm.chop! if comm[-1].ord.zero?
  comm.tr!("\0", ' ')
  result = File.read("/proc/#{pid}/smaps").split("\n")
  result.map! { |l| l[6..-1].to_i if l.start_with? SWAP }
  result.compact!
  s = result.reduce(:+).to_i
  [pid, s * 1024, comm]
rescue
  [pid, 0, nil]
end

def get_swap
  result = Dir.entries('/proc')
  result.map!     {|dir| get_swap_for dir unless dir.to_i.zero? }
  result.select!  {|s| s && s[1] > 0 }
  result.sort_by! {|x| x[1] }
  result
end

def main
  results = get_swap
  puts FORMAT % %w(PID SWAP COMMAND)
  results.each do |(pid, swap, comm)|
    puts FORMAT % [pid, filesize(swap), comm]
  end
  t = results.map {|x| x[1] }.reduce(:+).to_i
  puts TOTALFMT % filesize(t)
end

main
