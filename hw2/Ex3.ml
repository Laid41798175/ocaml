type heap = EMPTY
  | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank h =
  match h with
    EMPTY -> -1
    | NODE(r,_,_,_) -> r

let shake (x,lh,rh) =
  if (rank lh) >= (rank rh) then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge (h1, h2) =
  match (h1, h2) with
  (EMPTY, EMPTY) -> EMPTY
  | (EMPTY, _) -> h2
  | (_, EMPTY) -> h1
  | (NODE(_, v1, l1, r1), NODE(_, v2, l2, r2)) ->
    if (v1 < v2) then shake (v1, l1, merge (r1, h2))
    else shake (v2, l2, merge (h1, r2))

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin h =
  match h with
    EMPTY -> raise EmptyHeap
    | NODE(_,x,_,_) -> x

let deleteMin h =
  match h with
    EMPTY -> raise EmptyHeap
    | NODE(_,x,lh,rh) -> merge(lh,rh)

