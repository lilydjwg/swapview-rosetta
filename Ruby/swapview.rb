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

def get_swap_for pid
  comm = File.read("/proc/#{pid}/cmdline").tr("\x00", ' ').rstrip
  s = open("/proc/#{pid}/smaps") do |f|
    f.each_line.map { |l| $1.to_i if l =~ /^Swap: +([0-9]+)/}
     .compact
     .reduce :+
  end
  [pid, s * 1024, comm]
rescue
  [pid, 0, '']
end

def get_swap
  Dir.foreach('/proc')
     .select {|dir| dir =~ /^[0-9]+$/ }
     .map    {|pid| get_swap_for pid }
     .select {|s| s[1] > 0 }
     .sort   {|a, b| a[1] <=> b[1] }
end

def main
  results = get_swap
  puts FORMAT % %w(PID SWAP COMMAND)
  results.each do |psc|
    pid, swap, comm = *psc
    puts FORMAT % [pid, filesize(swap), comm]
  end
  t = results.map {|x| x[1] }.reduce(:+).to_i
  puts TOTALFMT % filesize(t)
end

if $0 == __FILE__
  main
end

