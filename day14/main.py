import sys
import hashlib
import concurrent.futures
import functools
import itertools as it

@functools.cache
def hash(salt, i=None):
    s = f'{salt}{(i if i is not None else "")}'
    return hashlib.md5(s.encode('utf-8')).hexdigest()

@functools.cache
def iterated_hash(salt, i, n_iterations):
    C = hash(salt, i)
    for _ in range(n_iterations - 1):
        C = hash(C)
    return C

def win(xs, n):
    t = it.tee(xs, n)
    for i in range(1, n):
        for e in t[i:]:
            next(e, None)
    return zip(*t)

def n_row(s, n, t=None):
    for tup in win(list(s), n):
        r = lambda acc, x: x if (x == t if t is not None else True) and acc == x else False
        if functools.reduce(r, tup):
            return tup[1]

def search(salt, c, i, n_iterations):
    for j in range(i+1,i+1000):
        h = iterated_hash(salt, j, n_iterations)
        if n_row(h, 5, c) is not None:
            return True
    return False

def solve(salt, n_iterations):
    C = 0
    for i in range(0, sys.maxsize):
        c = n_row(iterated_hash(salt, i, n_iterations), 3)
        if c is not None:
            if search(salt, c, i, n_iterations):
                C += 1
        if C == 64:
            return i


def solve_file(fp, executor):
    with open(fp, 'r') as f:
        salt = f.read().strip()
        F0 = executor.submit(solve, salt, 1)
        F1 = executor.submit(solve, salt, 2017)
        print(f'{fp} part 1 : {F0.result()}')
        print(f'{fp} part 2 : {F1.result()}')

if __name__ == '__main__':
    with concurrent.futures.ThreadPoolExecutor(max_workers=12) as executor:
        FS = [executor.submit(solve_file, fp, executor) for fp in sys.argv[1:]]
        for F in concurrent.futures.as_completed(FS):
            pass
