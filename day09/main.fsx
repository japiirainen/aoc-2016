open System

let parseMarker i (inp : string) =
  let p = inp[i+1..].Split [|'x'|]
  let p1 = p[1].Split [|')'|]
  let s = inp[i..].Split [|')'|]
  let markerLen = s[0].Length + 1
  let xs = inp[i+markerLen..i+markerLen-1+(int p[0])]
  (int p[0], int p1[0], xs, markerLen)

let mkDecode (f : bigint -> string -> bigint) (inp : string) : bigint =
  let rec go acc i =
    if i >= inp.Length then (acc : bigint) else
      let cur = inp[i]
      match cur with
      | '(' ->
        let (len, n, xs, markerLen) = parseMarker i inp
        let nn = bigint n
        go (acc + (f nn xs)) (markerLen + i + len)
      | c -> go (bigint 1 + acc) (i + 1)
  go (bigint 0) 0

let decode1 (inp : string) : bigint =
  let f (n : bigint) (xs : string) = n * (bigint xs.Length)
  mkDecode f inp

let rec decode2 (inp : string) : bigint =
  let f (n : bigint) (xs : string) = n * decode2 xs
  mkDecode f inp

let solveFile fp =
  printf "Solving for file : %s\n" fp
  let input = IO.File.ReadAllText fp
  let trimmed = input.Trim()
  printf "Part 1 : %A\n" (decode1 trimmed)
  printf "Part 1 : %A\n" (decode2 trimmed)

Environment.GetCommandLineArgs()[2..]
|> Array.iter solveFile

