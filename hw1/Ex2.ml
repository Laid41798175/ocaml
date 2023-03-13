type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec calc exp =
  match exp with
  NUM i -> i
  | PLUS (a, b) -> calc a + calc b
  | MINUS (a, b) -> calc a - calc b

let rec eval form =
  match form with
  TRUE -> true
  | FALSE -> false
  | NOT f -> if eval f then false else true
  | ANDALSO (f1, f2) -> if eval f1 then eval f2 else false
  | ORELSE (f1, f2) -> if eval f1 then true else eval f2
  | IMPLY (f1, f2) -> if eval f1 then eval f2 else true
  | LESS (e1, e2) -> calc e1 < calc e2