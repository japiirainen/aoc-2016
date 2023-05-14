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
 structure ILS = ListShow(ShowInt)

in

 val _ = log $ ILS.show [1, 2, 3]

end

val _ = Solver.run ()
