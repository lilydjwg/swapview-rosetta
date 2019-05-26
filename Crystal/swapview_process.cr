require "json"
require "system"

require "parallel"
require "./common.cr"

module SwapView
  def worker(in_channel, out_channel)
    loop do
      pid = in_channel.receive
      swap_info = get_swap_for(pid)
      # FIXME It is hard to share non-value types at the moment.
      # So we just drop the third element (cmd, String type).
      # See: Serialization for non-value types #5 https://github.com/RX14/parallel.cr/issues/5 .
      # And https://crystal-lang.org/api/0.28.0/Value.html .
      output = {swap_info[0], swap_info[1]}
      out_channel.send(output)
    end
  end

  def get_swap
    pids = get_pids

    job_channel = PChan(Int32).new
    results_channel = PChan(Tuple(Int32, Int32)).new

    System.cpu_count.times do
      pspawn { worker(job_channel, results_channel) }
    end

    spawn do
      pids.each { |pid| job_channel.send pid }
    end

    result = [] of {Int32, Int32, Nil}
    pids.size.times do
      swap_info = results_channel.receive
      next if !swap_info
      if swap_info[1] > 0
        result << {swap_info[0], swap_info[1], nil}
      end
    end

    result.sort_by! { |x| x[1] }
    result
  end

  def main
    results = get_swap
    report(results)
  end
end

SwapView.main
