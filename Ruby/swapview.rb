#!/usr/bin/env ruby
# frozen_string_literal: true

# for Ruby 2.4+

FORMAT = '%5s %9s %s'
TOTALFMT = 'Total: %8s'

def filesize(size)
  units = %w[B KiB MiB GiB TiB]
  left = size.abs
  num, unit = units.each_with_index do |_, i|
    l = left / 1024.0**i
    break l, i if (l <= 1100) || (i == units.length - 1)
  end
  if unit.zero?
    "#{size}B"
  else
    format('%.1f%s', size.negative? ? -num : num, units[unit])
  end
end

SWAP = 'Swap: '
# since strings (created from literal) are mutable, each iteration will create
# a new string instance if we put literal to map!{}
def get_swap_for(pid)
  comm = File.read("/proc/#{pid}/cmdline")
  comm.chop! if comm[-1] == "\0"
  comm.tr!("\0", ' ')
  s = File.read("/proc/#{pid}/smaps")
          .split("\n")
          .select { |l| l.start_with? SWAP }
          .map { |l| l[6..-1].to_i }
          .sum.to_i
  [pid, s * 1024, comm]
rescue StandardError
  [pid, 0, nil]
end

def get_swap
  Dir.entries('/proc')
     .map     { |dir| get_swap_for dir unless dir.to_i.zero? }
     .select  { |s| s && s[1].positive? }
     .sort_by { |x| x[1] }
end

results = get_swap
puts format(FORMAT, 'PID', 'SWAP', 'COMMAND')
results.each do |(pid, swap, comm)|
  puts format(FORMAT, pid, filesize(swap), comm)
end
t = results.map { |x| x[1] }.sum.to_i
puts TOTALFMT % filesize(t)
