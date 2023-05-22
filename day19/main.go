package main

import (
    "fmt"
    "os"
    "strings"
    "strconv"
)

func r(n int) []int {
    var xs []int
    for i := 1; i <= n; i++ {
        xs = append(xs, i)
    }
    return xs
}

func part1(xs []int) int {
    if len(xs) == 1 {
        return xs[0]
    } else {
        var ys []int
        var n int
        if len(xs) % 2 == 0 {
            n = 0
        } else {
            n = 2
        }
        for i := n; i < len(xs); i += 2 {
            ys = append(ys, xs[i])
        }
        return part1(ys)
    }
}

func part2(n int) int {
    var i = 1
    for i * 3 < n {
        i *= 3
    }
    return n - i
}

func solveFile(fp string) {
    b, _ := os.ReadFile(fp)
    n, _ := strconv.Atoi(strings.TrimSpace(string(b)))
    fmt.Printf("Solving for file : %s\n", fp)
    fmt.Printf("Part 1 : %d\n", part1(r(n)))
    fmt.Printf("Part 2 : %d\n", part2(n))
}

func main() {
    for _, fp := range os.Args[1:] {
        solveFile(fp)
    }
}
