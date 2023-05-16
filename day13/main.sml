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

in

val s0 = IS.singleton 1
val s1 = IS.add 3 s0
val s2 = IS.add 3 s0
val s3 = IS.addList [1, 2, 3, 4] s2
val s4 = IS.union (IS.singleton 69) s3
val s5 = IS.delete 2 s4

val _ = log $ ILS.show (IS.toList s5)

val _ = log $ ShowBool.show (IS.member 3 s5)

end

val _ = Solver.run ()
