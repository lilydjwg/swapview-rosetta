#!/usr/bin/env ruby
# frozen_string_literal: true

# for Ruby 2.4+

FORMAT = '%5s %9s %s'
TOTALFMT = 'Total: %8s'

UNITS = %w[B KiB MiB GiB TiB].freeze
def filesize(size)
  left = size.abs
  num, unit = UNITS.each_with_index do |_, i|
    l = left / 1024.0**i
    break l, i if (l <= 1100) || (i == UNITS.length - 1)
  end
  if unit.zero?
    "#{size}B"
  else
    format('%.1f%s', size.negative? ? -num : num, UNITS[unit])
  end
end

def get_swap_for(pid)
  comm = File.read("/proc/#{pid}/cmdline")
  comm.chop! if comm[-1] == "\0"
  comm.tr!("\0", ' ')
  s = File.read("/proc/#{pid}/smaps")
          .split("\n")
          .select { |l| l.start_with? 'Swap: ' }
          .map { |l| l[6..-1].to_i }
          .sum.to_i
  [s * 1024, pid, comm]
rescue StandardError
  [0, pid, nil]
end

def get_swap
  Dir.entries('/proc')
     .map     { |dir| get_swap_for dir unless dir.to_i.zero? }
     .select  { |s| s&.first&.positive? }
     .sort_by(&:first)
end

results = get_swap
puts format(FORMAT, 'PID', 'SWAP', 'COMMAND')
results.each do |(swap, pid, comm)|
  puts format(FORMAT, pid, filesize(swap), comm)
end
t = results.map(&:first).sum.to_i
puts TOTALFMT % filesize(t)
