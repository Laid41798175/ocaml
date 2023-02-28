type exp =
  Const of int
  | Minus of exp
  | Plus of exp * exp
  | Mult of exp * exp

let rec calc xp =
  match xp with
  | Const i -> i
  | Minus i -> (-1) * calc i
  | Plus (i, j) -> calc i + calc j
  | Mult (i, j) -> calc i * calc j

let a = calc (Plus (Const 1, Const 2))
let _ = print_endline (string_of_int a)