for (const fp of Deno.args) {
    await solveFile(fp)
}

async function solveFile(fp: string): Promise<void> {
    console.log(`Solving for file : ${fp}`)
    const ops = (await Deno.readTextFile(fp)).trim().split('\n').map(parseOperation)
    const part1 = 'abcdefgh'
    const part2 = 'fbgdceah'
    const go = (init: string) => ops.reduce(scramble, init)
    console.log(`Part 1 : ${go(part1)}`)
    console.log(`Part 2 : ${permutations(part2.split('')).find(perm => go(perm.join('')) === part2)!.join('')}`)
}

function scramble(pw: string, op: Operation): string {
    switch (op.kind) {
        case 'RotateRight': return rotateRight(pw, op.x)
        case 'RotateLeft': return rotateLeft(pw, op.x)
        case 'SwapPosition':  return set(op.x, pw[op.y], set(op.y, pw[op.x], pw))
        case 'SwapLetter': return pw.split('').map(a => a === op.x ? op.y : (a === op.y ? op.x : a)).join('')
        case 'RotateChar': return rotateChar(pw, op.x)
        case 'ReversePosition': return reversePosition(pw, op.x, op.y)
        case 'MovePosition': return movePosition(pw, op.x, op.y)
    }
}

type OpKind
  = 'RotateRight'
  | 'RotateLeft'

type Operation
  = {kind: 'RotateRight', x: number}
  | {kind: 'RotateLeft', x: number}
  | {kind: 'SwapPosition', x: number, y: number}
  | {kind: 'SwapLetter', x: string, y: string}
  | {kind: 'RotateChar', x: string}
  | {kind: 'ReversePosition', x: number, y: number}
  | {kind: 'MovePosition', x: number, y: number}

function parseOperation(s: string): Operation {
    const m = (y: string) => s.startsWith(y)
    const pi = (y: string) => parseInt(y, 10)
    const words = s.split(' ')
    if (m('swap position')) {
        return {kind: 'SwapPosition', x: pi(words[2]), y: pi(words[5])}
    } else if (m('swap letter')) {
        return {kind: 'SwapLetter', x: words[2], y: words[5]}
    } else if (m('rotate right')) {
        return {kind: 'RotateRight', x: pi(words[2])}
    } else if (m('rotate left')) {
        return {kind: 'RotateLeft', x: pi(words[2])}
    } else if (m('reverse')) {
        return {kind: 'ReversePosition', x: pi(words[2]), y: pi(words[4])}
    } else if (m('move')) {
        return {kind: 'MovePosition', x: pi(words[2]), y: pi(words[5])}
    } else if (m('rotate based')) {
        return {kind: 'RotateChar', x: words[6]}
    }
    throw new Error('Unreachable')
}

const swapLetter = (pw: string, x: string, y: string): string =>
    pw.split('').map(c => c === x ? y : c === y ? x : c).join('')

function rotateLeft(pw: string, x: number): string {
    const n = x % pw.length
    const cs = pw.split('')
    const [a, b] = splitAt(cs, n)
    return b.concat(a).join('')
}

function rotateRight(pw: string, x: number): string {
    const n = x % pw.length
    const cs = pw.split('')
    const [a, b] = splitAt(cs, pw.length - n)
    return b.concat(a).join('')
}

function set(i: number, x: string, xs: string): string {
    const [a, [_, ...b]] = splitAt(xs.split(''), i)
    return a.concat([x]).concat(b).join('')
}

function rotateChar(pw: string, x: string): string {
    const cs = pw.split('')
    const idx = cs.findIndex(c => c === x)
    return rotateRight(pw, (idx + (idx >= 4 ? 2 : 1)) % pw.length)
}

function reversePosition(pw: string, x: number, y: number): string {
    const cs = pw.split('')
    const [a, b] = splitAt(cs, x)
    const [c, d] = splitAt(b, y-x+1)
    c.reverse()
    return a.concat(c).concat(d).join('')
}

function movePosition(pw: string, i: number, j: number): string {
    const [a, [x, ...b]] = splitAt(pw.split(''), i)
    const [c, d] = splitAt(a.concat(b), j)
    return c.concat([x]).concat(d).join('')
}

function permutations<A>(xs: A[]): A[][] {
    const out: A[][] = []
    if (xs.length === 1) return [xs]
    for (const x of xs) {
        const ys = xs.filter(y => x !== y)
        for (const perm of permutations(ys)) {
            out.push([x].concat(perm))
        }
    }
    return out
}

function take<A>(n: number, xs: A[]): A[] {
    const out = []
    for (let i = 0; i < n; i++) out.push(xs[i])
    return out
}

function drop<A>(n: number, xs: A[]): A[] {
    const out = []
    for (let i = n; i < xs.length; i++) out.push(xs[i])
    return out
}

function splitAt<A>(xs: A[], n: number): [A[], A[]] {
    return [take(n, xs), drop(n, xs)]
}
