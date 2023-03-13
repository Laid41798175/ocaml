open Printf

let rec merge l1 l2 =
  match l1, l2 with
  | ([],[]) -> []
  | (hd::tl, []) -> l1
  | ([], hd::tl) -> l2
  | (hd1::tl1, hd2::tl2) ->
    if hd1 < hd2 then hd2::(merge (hd1::tl1) tl2)
    else hd1::(merge tl1 (hd2::tl2))

let list = merge [10; 8; 7; 6; 4] [9; 5; 3; 2; 1]
let _ = List.iter (printf "%d ") list
let _ = printf("\n")