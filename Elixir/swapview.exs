#!/usr/bin/env elixir

defmodule SwapView do
  defp filesize(size)         when size < 1100, do: "#{size}B"
  defp filesize(size),        do: filesize(size / 1024, ~w(KiB MiB GiB TiB))
  defp filesize(size, [h]),   do: "#{size |> Float.round(1)}#{h}"
  defp filesize(size, [h|_])  when size < 1100, do: "#{size |> Float.round(1)}#{h}"
  defp filesize(size, [_|t]), do: filesize(size / 1024, t)

  defp get_swap_for(pid) do
    try do
      comm = File.open!("/proc/#{pid}/cmdline", [:read], &IO.read(&1, :line))
      comm = comm |> binary_part(0, byte_size(comm) - 1) |> String.replace(<<0>>, " ")
      s = File.stream!("/proc/#{pid}/smaps", [:read])
      |> Stream.filter(&String.starts_with?(&1, "Swap:"))
      |> Stream.map(&(&1 |> String.split |> Enum.at(1) |> String.to_integer))
      |> Enum.reduce(0, &+/2)
      {pid, s * 1024, comm}
    rescue
      _ ->
        {:err, pid}
    end
  end


  defp get_swap do
    File.ls!("/proc")
    |> Stream.filter(fn pid -> pid =~ ~r/^[0-9]+$/ end)
    |> Stream.map(fn(x) -> Task.async(fn -> get_swap_for(x) end) end)
    |> Stream.map(fn(x) -> Task.await(x) end)
    |> Stream.filter(fn {_, s, _} when s > 0 -> true; _ -> false end)
    |> Enum.sort(fn ({_, s1, _}, {_, s2, _}) -> s1 < s2 end)
  end

  defp format({pid, size, comm}) do
    IO.puts "#{pid |> String.pad_leading(7)} #{size |> String.pad_leading(9)} #{comm}"
  end

  def main do
    result = get_swap()
    format {"PID", "SWAP", "COMMAND"}
    result |> Enum.each(fn {pid, size, comm} ->
      format {pid, size |> filesize, comm}
    end)
    total = result |> Enum.map(fn {_, size, _} -> size end) |> Enum.sum |> filesize
    IO.puts "Total: #{total |> String.pad_leading(8)}"
  end
end

SwapView.main
