type formula = TRUE | FALSE
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
  | NOT f -> not (eval f)
  | ANDALSO (f1, f2) -> if eval f1 then eval f2 else false
  | ORELSE (f1, f2) -> if eval f1 then true else eval f2
  | IMPLY (f1, f2) -> if eval f1 then eval f2 else true
  | LESS (e1, e2) -> calc e1 < calc e2

;;

assert (eval (IMPLY(FALSE, FALSE)) = true);
assert (eval (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY(TRUE, FALSE)))) = true);
assert ((eval (ANDALSO (TRUE, TRUE))) && (not (eval (ANDALSO (TRUE, FALSE)))) && (not (eval (ANDALSO (FALSE, TRUE)))) && (not (eval (ANDALSO (FALSE, FALSE)))) = true);
assert ((eval (ORELSE (TRUE, TRUE))) && (eval (ORELSE (TRUE, FALSE))) && (eval (ORELSE (FALSE, TRUE))) && (not (eval (ORELSE (FALSE, FALSE)))) = true);
assert ((eval (IMPLY (TRUE, TRUE))) && (not (eval (IMPLY (TRUE, FALSE)))) && (eval (IMPLY (FALSE, TRUE))) && (eval (IMPLY (FALSE, FALSE))) = true);
assert (eval (LESS (NUM 3, NUM 5)) = true);
assert (eval (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2))) = false);
assert (eval (LESS (MINUS (NUM 3, NUM 5), MINUS (NUM 1, NUM 2))) = true);
assert (eval (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE)) = false);
assert (eval (IMPLY(LESS (NUM 1, NUM 0), ANDALSO(TRUE, ORELSE(NOT TRUE, LESS(NUM 2, NUM 1))))) = true);