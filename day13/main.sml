signature SOLVER =
 sig
   val run : unit -> unit
 end

structure Solver : SOLVER =
  struct
    open Prelude

    structure Search = SearchFun(IntTupleOrd)

    type entry = {steps : int, coord : int * int}

    fun formula i (x, y) = (x*x + 3*x + 2*x*y + y + y*y + i)

    fun isValidCoord i (x, y) =
      x >= 0 andalso y >= 0 andalso
      even (popCount (dec2bin (formula i (x, y))))

    fun cardinal (x, y) : (int * int) list =
      [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]

    fun nextEntries (i : int) (e : entry) =
      List.map
        (fn c => {steps = (#steps e) + 1, coord = c})
        (List.filter (isValidCoord i) (cardinal (#coord e)))

    fun p1 entries =
      List.hd
        (List.filter
          (fn e => (case IntTupleOrd.compare (#coord e) (31, 39)
                     of EQ => true
                      | LT => false
                      | GT => false))
           entries)

    fun solve i = let
      val initialEntry : entry = {steps = 0, coord = (1, 1)}

      fun rep (e : entry) = #coord e

      fun next inp = fn (e : entry) => nextEntries inp e

      val entries = Search.bfsOn rep (next i) [initialEntry]

      val resp1 = ShowInt.show (#steps (p1 entries))

      val resp2 = ShowInt.show
                    (List.length $
                      takeWhile (fn x => x <= 50)
                        (map (fn (e : entry) => #steps e) entries))
    in
      log $ "Part 1 : " ^ resp1 ^ "\n" ^ "Part 2 : " ^ resp2
    end

    fun solveFile fp =
      let val _ = log $ "Solving for file : " ^ fp in
      let val input = readFile fp in
      case Int.fromString input of
        NONE => log $ "Bad input, expecting value of type int but got : " ^ input
      | SOME i => solve i
      end
    end

    fun run () = let val () = iter solveFile argv in exitSuccess () end

  end

open Solver

val _ = run ()

