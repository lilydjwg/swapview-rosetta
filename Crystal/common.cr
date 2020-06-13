module SwapView
  extend self

  FORMAT = "%7s %9s %s"
  TOTALFMT = "Total: %8s"
  SWAP = "Swap: "

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

  def get_pids
    pids = [] of Int32
    Dir.entries("/proc").each do |entry|
      pid = entry.to_i?
      if pid
        pids << pid
      end
    end
    pids
  end

  def get_swap_for(pid)
    comm = File.read("/proc/#{pid}/cmdline")
    comm = comm.rchop if comm != "" && comm[-1] == '\0'
    comm = comm.tr("\0", " ")
    result = File.read("/proc/#{pid}/smaps").split('\n')
    result.select! { |l| l.starts_with? SWAP }
    if result.empty?
      {pid, 0, nil}
    else
      res = result.map { |l| l[6..-3].to_i }
      s = res.reduce { |acc, i| acc + i }
      {pid, s * 1024, comm}
    end
  rescue File::AccessDeniedError | File::NotFoundError
    {pid, 0, nil}
  end

  def report(results)
    puts FORMAT % %w(PID SWAP COMMAND)
    results.each do |value|
      pid, swap, comm = value
      puts FORMAT % [pid, filesize(swap), comm]
    end
    res = results.map { |x| x[1] }
    if res.empty?
      t = 0
    else
      t = res.reduce { |acc, i| acc + i }
    end
    puts TOTALFMT % filesize(t)
  end
end
