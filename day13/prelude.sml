infix 4 $

signature SHOW =
  sig
    type t
    val show : t -> string
  end

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
    val insert : 'a -> 'a queue -> 'a queue
    val remove : 'a queue -> 'a * 'a queue
    val addList : 'a list -> 'a queue -> 'a queue
  end

structure Queue : QUEUE =
 struct
   type 'a queue = 'a list * 'a list
   exception Empty
   val empty = (nil, nil)
   fun insert x (b, f) = (x::b, f)
   fun remove (nil, nil) = raise Empty
     | remove (bs, nil) = remove (nil, rev bs)
     | remove (bs, f::fs) = (f, (bs, fs))
   fun addList is q =
     List.foldl (fn (x, queue) => insert x queue) q is
 end

datatype order = LT | GT | EQ

structure OrderShow : SHOW =
  struct
    type t = order
    fun show LT = "LT"
      | show GT = "GT"
      | show EQ = "EQ"
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

structure ShowInt : SHOW =
  struct
    type t = int
    fun show x = Int.toString x
  end

structure ShowBool : SHOW =
  struct
    type t = bool
    fun show true = "true"
      | show false = "false"
  end

functor ShowList (S : SHOW) : SHOW =
  struct
    type t = S.t list
    fun show xs =
      let fun go []           = ""
            | go (elem::[])   = S.show elem
            | go (elem::rest) = (S.show elem) ^ ", " ^ (go rest)
        in "[ " ^ go xs ^ " ]"
      end
  end

signature SET =
  sig
    structure Ord : ORDERED
    type item = Ord.t
    type set

    val empty : set
    val singleton : item -> set
    val toList : set -> item list
    val add : item -> set -> set
    val addList : item list -> set -> set
    val delete : item -> set -> set
    val member : item -> set -> bool
    val union : set -> set -> set
  end

functor SetFun
  (O : ORDERED) :>
    SET where type Ord.t = O.t =
  struct
    structure Ord = O
    type item = Ord.t
    type set = item list

    exception NotFound

    val empty = []
    fun singleton x = [x]
    fun toList s = s
    fun add i s = let
      fun go [] = [i]
        | go (elem::rest) = (case Ord.compare i elem
          of LT => i :: elem :: rest
           | EQ => i :: rest
           | GT => elem :: (go rest))
    in
      go s
    end
    fun union s0 s1 = let
      fun merge [] l1 = l1
        | merge l0 [] = l0
        | merge (x::r0) (y::r1) = (case Ord.compare x y
        of LT => x :: (merge r0 (y::r1))
         | EQ => x :: (merge r0 r1)
         | GT => y :: (merge (x::r0) r1))
    in
      merge s0 s1
    end
    fun addList is s = let
      val items = List.foldl (fn (x, set) => add x set) [] is
    in
      union s items
    end
    fun delete i s = let
      fun go _ [] = raise NotFound
        | go prefix (elem::rest) = (case Ord.compare i elem
        of LT => raise NotFound
         | EQ => List.revAppend(prefix, rest)
         | GT => go (elem::prefix) rest)
    in
      go [] s
    end
    fun member i s = let
      fun go [] = false
        | go (elem::rest) = (case Ord.compare i elem
        of LT => false
         | EQ => true
         | GT => go rest)
    in
      go s
    end
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
  (O : ORDERED) :>
    SEARCH where type Rep.t = O.t =
  struct
    structure Rep : ORDERED = O
    structure S : SET = SetFun(O)
    structure Q : QUEUE = Queue

    fun bfsOn rep next start = let
      fun loop seen queue = (let
        val (x, q) = Q.remove queue
        val r = rep x
        val seen' = S.add r seen
        val q' = Q.addList (next x) q
      in
        if (S.member r seen)
        then loop seen q
        else x :: loop seen' q'
      end) handle Q.Empty => []
    in
      loop S.empty (Q.addList start Q.empty)
    end
  end
