signature SOLVER =
 sig
   val solveFile : string -> unit
   val run : unit -> unit
 end

structure Solver : SOLVER =
  struct
    open Prelude

    fun solveFile fp =
      let val _ = log $ "Solving for file : " ^ fp in
      let val input = readFile fp in
      case Int.fromString input of
        NONE => log $ "Bad input, expecting value of type int but got : " ^ input
      | SOME i => log $ "got int " ^ (Int.toString i)
      end
    end

    fun run () = let val () = iter solveFile argv in exitSuccess () end

  end

(* testing *)
local

  open Prelude
  structure ILS = ShowList(ShowInt)

  structure IS = SetFun(IntOrd)

  structure Q = Queue

in

val _ = log "----------SET---------"

val s0 = IS.singleton 1
val s1 = IS.add 3 s0
val s2 = IS.add 3 s0
val s3 = IS.addList [1, 2, 3, 4] s2
val s4 = IS.union (IS.singleton 69) s3
val s5 = IS.delete 2 s4

val _ = log $ ILS.show (IS.toList s5)

val _ = log $ ShowBool.show (IS.member 3 s5)

val _ = log "----------QUEUE---------"

val q0 = Q.insert 3 Q.empty
val q1 = Q.insert 69 q0
val q2 = Q.addList [4, 5, 1] q1

val (top, q3) = Q.remove q2

val _ = log $ ShowInt.show top

val (top1, _) = Q.remove q3

val _ = log $ ShowInt.show top1

val _ = log "----------SEARCH---------"

structure S = SearchFun(IntOrd)

val rep = fn x => x

val next = fn x => if x > 0 then [x - 1, x - 2] else []

val start = [10]

val _ = log $ ILS.show (S.bfsOn rep next start)

end

val _ = Solver.run ()
