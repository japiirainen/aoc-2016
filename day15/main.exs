defmodule Solver do
  def parse_line(line) do
    Regex.run(~r/Disc #(\d+).* (\d+) positions.* (\d+)./, line)
    |> Enum.drop(1)
    |> Enum.map(&Integer.parse(&1))
    |> Enum.map(fn x ->
      case x do
        {i, _} -> i
      end
    end)
  end

  def falls?(t, discs) do
    discs
    |> Enum.map(fn [disc, pos, at] -> rem(at + t + disc, pos) == 0 end)
    |> Enum.all?()
  end

  def solve(discs) do
    Stream.unfold(0, fn t ->
      if !falls?(t, discs) do
        {:ok, t + 1}
      else
        nil
      end
    end)
    |> Enum.to_list()
    |> Enum.count()
  end

  def run(fp) do
    IO.puts("Solving for file : #{fp}")
    {:ok, input} = File.read(fp)

    discs =
      input
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&parse_line(&1))

    IO.puts("Part 1 : #{solve(discs)}")
    IO.puts("Part 2 : #{solve(discs |> Enum.concat([[7, 11, 0]]))}")
  end
end

System.argv() |> Enum.map(&Solver.run(&1))
