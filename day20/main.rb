def remove_overlap(blacklist)
  def go(bs)
    case bs
      in [[lo0, hi0], [lo1, hi1], *rest]
        if hi0 >= lo1 - 1
          go [[lo0, [hi0, hi1].max], *rest]
        else
          [[lo0, hi0]] + go([[lo1, hi1]] + rest)
        end
      in [x, *xs]
        [x] + go(xs)
      in []
        []
    end
  end
  go blacklist.sort
end

def lowest(blacklist)
  case blacklist
    in [[lo, hi], *rest]
      if lo == 0
        hi + 1
      else
        lo - 1
    end
  end
end

def count_valid(blacklist)
  2 ** 32 - blacklist.map { |range| range[1]-range[0]+1 }.sum
end

def solve_file(fp)
  puts "Solving for file : #{fp}"
  blacklist = File.readlines(fp).map { |l| l.strip.split("-").map(&:to_i) }
  blacklist = remove_overlap blacklist
  puts "Part 1 : #{lowest blacklist}"
  puts "Part 2 : #{count_valid blacklist}"
end

ARGV.each do |fp|
  solve_file fp
end
