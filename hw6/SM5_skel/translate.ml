(* PL HW6 translate.ml
     SNUCSE 18 OH, JINSU
   2018-19857
*)

(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct

  let malloc x = [Sm5.MALLOC; Sm5.BIND x]
  let define f x sm = [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ sm)); Sm5.BIND f]
  let free = [Sm5.UNBIND; Sm5.POP]

  (* x has to be declared before. if not, malloc x first *)
  let assign x = [Sm5.PUSH (Sm5.Id x); Sm5.STORE]
  
  let get x = [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
  (* when you have to return x which is variable *)
  let return x = get x

  (* call includes return f(x) *)
  (* call by reference basically *)
  let call f x =
    [Sm5.PUSH (Sm5.Id f)]
    @ [Sm5.PUSH (Sm5.Id f)]
    @ get x
    @ [Sm5.PUSH (Sm5.Id x)]
    @ [Sm5.CALL]

  (* TODO : complete this function  *)
  (* Function - Only one parameter *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))] 
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e -> trans e @ [Sm5.NOT]
    | K.ASSIGN (x, e) ->
      trans e @ assign x (* x := e *)
      @ return x
    | K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
    | K.IF (e1, e2, e3) -> trans e1 @ [Sm5.JTR (trans e2, trans e3)]
    | K.WHILE (e1, e2) -> failwith "Unimplemented WHILE"
    | K.FOR (i, e1, e2, e3) -> failwith "Unimplemented FOR"
    | K.LETV (x, e1, e2) ->
      trans e1 @ malloc x @ assign x (* let x := e1 *)
      @ trans e2 @ free (* in e2 *)
    | K.LETF (f, x, e1, e2) ->
      define f x (trans e1) (* let f(x) := e1 *)
      @ trans e2 @ free (* in e2 *)
    | K.CALLV (f, e) ->
      (* temporary declare #x and CALLR with #x *)
      trans e @ malloc "#x" @ assign "#x" (* let #x := e *)
      @ call f "#x" @ free (* in f(#x) *)
    | K.CALLR (f, y) ->
      call f y (* f(y) *)
    | K.READ x ->
      [Sm5.GET] @ assign x (* READ *)
      @ return x (* x := READ() *)
    | K.WRITE e ->
      trans e @ malloc "#x" @ assign "#x" (* let #x := e *)
      @ get "#x" @ [Sm5.PUT] (* WRITE *)
      @ return "#x" @ free (* in WRITE(#x) *)
    
end
