import Foundation

func bfsOn<A, R: Hashable>(rep: @escaping (A) -> R,
                           next: @escaping (A) -> Array<A>,
                           ini: A) -> (((A) -> Bool) -> A?) {
    return { done in
        var seen: Set<R> = []
        var q: Array<A> = [ini]
        while q.count > 0 {
            let x = q.last!
            if done(x) { return x }
            q.removeLast()
            let r = rep(x)
            if !seen.contains(r) {
                for v in next(x) {
                    q.insert(v, at: 0)
                }
                seen.insert(r)
            }
        }
        return nil }
}

typealias Coord = (Int, Int)

func cardinal(_ c: Coord) -> Array<Coord> {
    let (x, y) = c
    return [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
}

typealias Maze = Array<Array<((Int, Int), Character)>>
typealias State = (Set<Int>, Coord, Int)

func next(_ maze: Maze) -> ((State) -> Array<State>) {
    return { cur in
        let (seen, here, steps) = cur
        return cardinal(here)
            .flatMap{ heree in
                let (r, c) = heree
                let (_, x) = maze[r][c]
                if x != "#" {
                    let i: Int? = Int(String(x))
                    var seenn = seen
                    if (i != nil) { seenn.insert(i!) }
                    let stepss = steps + 1
                    return [(seenn, heree, stepss)]
                }
                return [] } }
}

struct Entry : Hashable {
    var x: Int
    var y: Int
    var q: Set<Int>
}

func solveFile(_ filePath: String) -> () {
    print("Solving for file : \(filePath)")

    let contents = (try! String(contentsOfFile: filePath))
        .trimmingCharacters(in: .whitespacesAndNewlines)

    let targets = Set(Array(contents).flatMap { c in let i: Int? = Int(String(c)); return i == nil ? [] : [i!] })

    let lines = contents.split(separator: "\n")

    let maze: Maze = lines
        .enumerated()
        .map { (row, xs) in
            Array(xs)
                .enumerated()
                .map { (col, y) in ((row, col), y) } }

    let start = maze
        .joined()
        .first { (_, e) in e == "0" }
        .map { (c, _) in c }!

    let ini: State = ([0], start, 0)
    let solve = bfsOn(rep: { (x) in
                             let (seen, here, _): State = x
                             return Entry(x: here.0, y: here.1, q: seen) },
                      next: next(maze),
                      ini: ini)

    print("Part 1 : \(solve({ a in a.0 == targets })!.2)")
    print("Part 2 : \(solve({ a in a.0 == targets && a.1 == start })!.2)")
}

for filePath in CommandLine.arguments[1...] {
    solveFile(filePath)
}
