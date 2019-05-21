require "json"
require "system"
require "./common.cr"

CPU_COUNT = System.cpu_count

module SwapView
  def get_swap
    pids = get_pids

    processes = pids.map {|pid| Process.fork {
      swap_info = get_swap_for(pid)
      puts swap_info.to_json
    }}
    processes.each {|process| process.wait }
    result = [] of {Int32, Int32, String | Nil}
    processes.each do |process|
      op = process.output?
      next if !op
      output = JSON.parse(op)
      size = output[1].as_i
      if size > 0
        result << {output[0].as_i, size, output[2].as_s?}
      end
    end

    result.sort_by! {|x| x[1] }
    result
  end

  def main
    results = get_swap
    report(results)
  end
end

SwapView.main
