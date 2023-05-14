infix 4 $

signature PRELUDE =
  sig
    (* IO *)
    val log : string -> unit
    val argv : string list
    val readFile : string -> string
    val exitSuccess : unit -> unit

    val iter : ('a -> unit) -> 'a list -> unit
    val $ : ('a -> 'b) * 'a -> 'b
  end

structure Prelude : PRELUDE =
  struct
    fun log s = print (s ^ "\n")
    val argv = CommandLine.arguments();
    fun readFile fp = TextIO.inputAll (TextIO.openIn fp)
    fun exitSuccess () = OS.Process.exit OS.Process.success

    fun iter f xs = let val _ =  map f xs in () end
    fun f $ a = f a
  end

signature QUEUE =
  sig
    type 'a queue = 'a list * 'a list
    exception Empty
    val empty : 'a queue
    val insert : 'a * 'a queue -> 'a queue
    val remove : 'a queue -> 'a * 'a queue
  end

structure Queue : QUEUE =
 struct
   type 'a queue = 'a list * 'a list
   exception Empty
   val empty = (nil, nil)
   fun insert (x, (b, f)) = (x::b, f)
   fun remove (nil, nil) = raise Empty
     | remove (bs, nil) = remove (nil, rev bs)
     | remove (bs, f::fs) = (f, (bs, fs))
 end

datatype order = LT | GT | EQ

signature ORDER =
  sig
    val toString : order -> string
  end

structure Order =
  struct
    fun toString LT = "LT"
      | toString GT = "GT"
      | toString EQ = "EQ"
  end

signature ORDERED =
  sig
    type t
    val compare : t -> t -> order
  end

structure IntOrd : ORDERED =
  struct
    type t = int
    fun compare a b = if a > b then GT else (if a < b then LT else EQ)
  end

signature SET =
  sig
    structure Ord : ORDERED
    type item = Ord.t
    type set

    val empty : set
    val singleton : item -> set
    val toList : set -> item list
  end

signature SHOW =
  sig
    type t
    val show : t -> string
  end

structure ShowInt : SHOW =
  struct
    type t = int
    fun show x = Int.toString x
  end

functor ListShow (S : SHOW) : SHOW =
  struct
    type t = S.t list

    fun show xs =
      let fun go []           = ""
            | go (elem::[])   = S.show elem
            | go (elem::rest) = (S.show elem) ^ ", " ^ (go rest)
        in
        "[ " ^ go xs ^ " ]"
      end

  end

functor SetFun
  (structure O : ORDERED) :>
    SET where type Ord.t = O.t =
  struct
    structure Ord = O
    type item = Ord.t
    type set = item list

    val empty = []
    fun singleton x = [x]
    fun toList s = s
  end

signature SEARCH =
  sig
    structure Rep : ORDERED
    val bfsOn :
      ('a -> Rep.t)   -> (* representative function *)
      ('a -> 'a list) -> (* successor state generator *)
      'a list         -> (* initial states *)
      'a list            (* reachable states *)
  end

functor SearchFun
  (structure O : ORDERED) :>
    SEARCH where type Rep.t = O.t =
  struct
    structure Rep : ORDERED = O
    fun bfsOn rep suc init =
      (* TODO: this is going to need some sort of queue and set datatypes *)
      nil
  end
