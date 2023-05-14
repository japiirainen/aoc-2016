signature PRELUDE =
  sig
    (* IO *)
    val log : string -> unit
    val argv : string list
    val readFile : string -> string
    val exitSuccess : unit -> unit

    val iter : ('a -> unit) -> 'a list -> unit
  end

structure Prelude : PRELUDE =
  struct
    fun log s = print (s ^ "\n")
    val argv = CommandLine.arguments();
    fun readFile fp = TextIO.inputAll (TextIO.openIn fp)
    fun exitSuccess () = OS.Process.exit OS.Process.success

    fun iter f xs = let val _ =  map f xs in () end
  end

signature SOLVER =
 sig
   val solveFile : string -> unit
   val run : unit -> unit
 end

structure Solver : SOLVER =
  struct
    open Prelude;

    fun solveFile fp =
      let val _ = log ("Solving for file : " ^ fp) in
      let val input = readFile fp in
      case Int.fromString input of
        NONE => log ("Bad input, expecting value of type int but got : " ^ input)
      | SOME i => log ("got int " ^ (Int.toString i))
      end
    end

    fun run () = let val () = iter solveFile argv in exitSuccess () end

  end

val _ = Solver.run ()
