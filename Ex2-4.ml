type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let shake (x,lh,rh) = if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge 

let rank h =
  match h with
| EMPTY -> -1
| NODE(r, , , ) -> r

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY))

let findMin h = match h with
| EMPTY -> raise EmptyHeap
| NODE( ,x, , ) -> x

let deleteMin h = match h with
| EMPTY -> raise EmptyHeap
| NODE( ,x,lh,rh) -> merge(lh,rh)

