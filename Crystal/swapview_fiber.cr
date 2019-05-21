require "./common.cr"

module SwapView
  def get_swap
    pids = get_pids
    outputs = parallel(
      pids.each {|pid| get_swap_for(pid) }
    )

    result = [] of Tuple(Int32, Int32, String | Nil)
    outputs.each do |output|
      if output && output[1] > 0
        result << output
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
