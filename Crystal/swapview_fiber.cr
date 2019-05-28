require "./common.cr"

module SwapView
  def get_swap
    pids = get_pids

    channel = Channel(Tuple(Int32, Int32, String | Nil)).new(pids.size)

    pids.each do |pid|
      spawn do
        swap_info = get_swap_for(pid)
        channel.send(swap_info)
      end
    end

    result = [] of {Int32, Int32, String | Nil}
    pids.size.times do
      swap_info = channel.receive
      if swap_info && swap_info[1] > 0
        result << swap_info
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
