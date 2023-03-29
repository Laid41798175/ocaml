(* PL HW2 Ex1.ml
   SNUCSE 18 OH, JINSU (오진수)
   2018-19857
*)

let rec iter (n, f) =
  if n <= 0 then fun x -> x
  else fun x -> f (iter ((n - 1), f) x)