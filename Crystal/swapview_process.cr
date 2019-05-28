require "system"

require "parallel_worker"
require "./common.cr"

module SwapView
  def get_swap
    pids = get_pids

    worker = ParallelWorker::Process(Int32, Tuple(Int32, Int32, String | Nil)).new(System.cpu_count.to_i32) do |pid|
      get_swap_for(pid)
    end

    result = worker.perform_all(pids)
    result.select!  { |swap_info| swap_info && swap_info[1] > 0 }

    result.sort_by! { |x| x[1] }
    result
  end

  def main
    results = get_swap
    report(results)
  end
end

SwapView.main
