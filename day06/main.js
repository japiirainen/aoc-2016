const fs = require("fs")

const T = xs => xs[0].map((_, r) => xs.map((x) => x[r]))

Map.prototype.fromListWith = combine => xs =>
  xs.reduce((m, [k, v]) => m.has(k) ? m.set(k, combine(v)(m.get(k))) : m.set(k, v), new Map())

Map.prototype.maxBy = f => m => {
  const es = Array.from(m.entries())
  return es.slice(1)
           .reduce(([mk, mv], [k, v]) => f(v)(mv) === 1 ? [k, v] : [mk, mv], es[0])
}

const solve = fp => compare =>
  T(
    fs.readFileSync(fp, 'utf-8')
      .trim()
      .split('\n')
      .map(l => l.split(''))
  )
    .map(xs =>
      Map.prototype.fromListWith(x => y => x + y)(xs.map(x => [x, 1]))
    )
    .map(Map.prototype.maxBy(compare))
    .map(([k, _]) => k)
    .join('')

process.argv.slice(2).forEach(fp => {
  console.log(`Solving for file : ${fp}
Part 1 : ${solve(fp)(x => y => x > y ? 1 : 0)}
Part 2 : ${solve(fp)(x => y => x > y ? 0 : 1)}`)
})

