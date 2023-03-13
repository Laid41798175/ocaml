open Printf
type expr =
  NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list

let rec eval exp =
  match exp with
  NUM i -> i
  | PLUS (e1, e2) -> (eval e1) + (eval e2)
  | MINUS (e1, e2) -> (eval e1) - (eval e2)
  | MULT (e1, e2) -> (eval e1) * (eval e2)
  | DIVIDE (e1, e2) -> (eval e1) / (eval e2)
  | MAX l -> 
    match l with
    [] -> 0
    | hd::tl -> List.fold_left (fun max i -> if max < (eval i) then (eval i) else max) (eval hd) tl

let a = eval (PLUS ((MINUS ((MAX [(NUM (-1)); (MULT ((NUM 2), (NUM (-8)))); (NUM (-6))]), (NUM 4))), (DIVIDE ((NUM 6), (MULT ((NUM 3), (NUM 2)))))))
let _ = printf ("%d\n") a