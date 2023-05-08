R = 6
C = 50

A = zeros(Int8, R, C)

function solve_file(fp :: String)
    println("Solving for file $fp")
    for line in readlines(open(fp, "r"))
        ss = split(line, " ")
        cmd = ss[1]
        arg = ss[2]
        nums = [parse(Int64, m.match) for m in eachmatch(r"[0-9]+", line)]
        if cmd == "rect"
            A[1:nums[2], 1:nums[1]] .= 1
        else
            if arg == "column"
                A[:, nums[1]+1] = circshift(A[:, nums[1]+1], nums[2])
            else
                A[nums[1]+1, :] = circshift(A[nums[1]+1, :], nums[2])
            end
        end
    end

    println("Part 1 : $(sum(A))")
    println("Part 2 : ")
    for r in 1:R
        for c in 1:C
            if A[CartesianIndex(r, c)] == 1
                print('@')
            else
                print(' ')
            end
        end
        print('\n')
    end
end

for fp in ARGS
    solve_file(fp)
end

