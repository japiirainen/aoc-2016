open Eio.Std

module Env = Eio.Stdenv

module Machine = struct
  type registers =
    { a : int;
      b : int;
      c : int;
      d : int }

  type register = A | B | C | D

  let update_reg f regs = function
    | A -> { a = f regs.a; b = regs.b; c = regs.c; d = regs.d }
    | B -> { a = regs.a; b = f regs.b; c = regs.c; d = regs.d }
    | C -> { a = regs.a; b = regs.b; c = f regs.c; d = regs.d }
    | D -> { a = regs.a; b = regs.b; c = regs.c; d = f regs.d } ;;

  let set_reg v regs reg = update_reg (fun _ -> v) regs reg ;;

  let reg_at rs = function
    | A -> rs.a
    | B -> rs.b
    | C -> rs.c
    | D -> rs.d ;;

  let zero_registers: registers =
    { a = 0; b = 0; c = 0; d = 0 } ;;

  type value = Int of int | Reg of register

  type instr =
    | Copy of value * register
    | Inc of register
    | Dec of register
    | Jnz of value * value
    | Tgl of value

  exception RuntimeError of string ;;

  let exec instructions a =
    let ( @@ ) a n = try Some (Array.get a n) with _ -> None in

    let rval rs: value -> int = function
      | Int i -> i
      | Reg r -> reg_at rs r in

    let rec go (ip: int) (regs: registers) =

      let toggle at =
        match instructions @@ at with
        | None -> ()
        | Some(i) ->
          Array.set instructions at (match i with
          | Inc(r) -> Dec r
          | Dec(r) -> Inc r
          | Jnz(x, Reg(y)) -> Copy (x, y)
          | Tgl(Reg(x)) -> Inc x
          | Copy(v, r) -> Jnz (v, (Reg r))
          | _ -> raise (RuntimeError "Nonsensical toggle instruction")) in

      let go_instr = function
        | Copy(v, r) -> (ip + 1, set_reg (rval regs v) regs r)
        | Inc(r) -> (ip + 1, update_reg (fun v -> v + 1) regs r)
        | Dec(r) -> (ip + 1, update_reg (fun v -> v - 1) regs r)
        | Jnz(v, i) -> (ip + (if (rval regs v) = 0 then 1 else (rval regs i)), regs)
        | Tgl(v) -> toggle (ip + (rval regs v)); (ip + 1, regs) in

      match instructions @@ ip with
      | None -> regs.a
      | Some i -> let (ip', regs') = go_instr i in go ip' regs' in

    go 0 (set_reg a zero_registers A) ;;
end

module Parser = struct
  open String ;;
  open Machine ;;

  exception BadInput of string ;;

  let ( @ ) n xs = List.nth n xs ;;

  let parse_reg = function
    | "a" -> A
    | "b" -> B
    | "c" -> C
    | "d" -> D
    | _ -> raise (BadInput "Invalid register") ;;

  let parse_int s =
    try Some (int_of_string s) with _ -> None ;;

  let parse_value s =
    match parse_int s with
    | None -> Reg (parse_reg s)
    | Some i -> Int i ;;

  let parse_instruction line =
    let ps = split_on_char ' ' line in
    match (ps @ 0) with
    | "cpy" -> Copy (parse_value (ps @ 1), parse_reg (ps @ 2))
    | "jnz" -> Jnz (parse_value (ps @ 1), parse_value (ps @ 2))
    | "tgl" -> Tgl (parse_value (ps @ 1))
    | "inc" -> Inc (parse_reg (ps @ 1))
    | "dec" -> Dec (parse_reg (ps @ 1))
    | _ -> raise (BadInput "Invalid instruction") ;;

  let parse input =
    let lines = split_on_char '\n' @@ trim input in
    let instructions = List.map parse_instruction lines in
    Array.of_list instructions ;;
end

let solve_file env file_path =
  let ( / ) = Eio.Path.( / ) in
  let path = Env.cwd env / file_path in

  let input = Eio.Path.load path in
  let instructions = Parser.parse input in

  let solve part n =
    traceln "File = %s, Part = %d : %d" file_path part
    (Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env)
      (fun () -> (Machine.exec (Array.copy instructions) n))) in

  Fiber.both
    (fun () -> solve 1 7)
    (fun () -> solve 2 12) ;;

exception NotEnoughArguments ;;

let () =
  Eio_main.run @@ fun env ->
  let file_paths = match Array.to_list Sys.argv with | _::xs -> xs | _ -> raise NotEnoughArguments in
  Fiber.List.iter (solve_file env) file_paths ;;

