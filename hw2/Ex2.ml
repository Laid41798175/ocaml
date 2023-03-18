exception FreeVariable

type exp = X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

let rec sigmaInt a b f =
  if (a <= b) then (f (float_of_int a)) +. sigmaInt (a + 1) b f
  else 0.

let sigma a b f =
  let i = (int_of_float a)
  and j = (int_of_float b)
  in sigmaInt i j f

let rec integralGrt a b f =
  if (a +. 0.1 > b) then 0.
  else ((f a) *. 0.1) +. (integralGrt (a +. 0.1) b f)

let integral a b f =
  if (a <= b) then integralGrt a b f
  else (-1.) *. integralGrt b a f

let rec fx expr x =
  match expr with
  X -> x
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (fx e1 x) +. (fx e2 x)
  | SUB (e1, e2) -> (fx e1 x) -. (fx e2 x)
  | MUL (e1, e2) -> (fx e1 x) *. (fx e2 x)
  | DIV (e1, e2) -> (fx e1 x) /. (fx e2 x)
  | SIGMA (e1, e2, e3) -> sigma (calculate e1) (calculate e2) (fx e3)
  | INTEGRAL (e1, e2, e3) -> integral (calculate e1) (calculate e2) (fx e3)
and calculate expr =
  match expr with
  X -> raise FreeVariable
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> calculate e1 +. calculate e2
  | SUB (e1, e2) -> calculate e1 -. calculate e2
  | MUL (e1, e2) -> calculate e1 *. calculate e2
  | DIV (e1, e2) -> calculate e1 /. calculate e2
  | SIGMA (e1, e2, e3) -> sigma (calculate e1) (calculate e2) (fx e3)
  | INTEGRAL (e1, e2, e3) -> integral (calculate e1) (calculate e2) (fx e3)