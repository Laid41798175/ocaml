(* PL HW4 Ex4.ml
   SNUCSE 18 OH, JINSU
   2018-19857
*)

type require = id * (cond list)
and cond = Items of gift list (* giftOf *)
  | Same of id (* same giftOf as for id *)
  | Common of cond * cond (* giftOf of common conditions *)
  | Except of cond * gift list (* exclude giftOf *)
and gift = int (* gift number *)
and id = A | B | C | D | E (* id names *)

let giftOf (ide, allList) =
  match ide with
  | A -> List.nth allList 0
  | B -> List.nth allList 1
  | C -> List.nth allList 2
  | D -> List.nth allList 3
  | E -> List.nth allList 4

let rec common (l1, l2) =
  match l2 with
  | [] -> []
  | hd::tl ->
    if (List.mem hd l1) then hd::(common (l1, tl))
    else (common (l1, tl))

let rec except (l1, l2) =
  match l1 with
  | [] -> []
  | hd::tl ->
    if (List.mem hd l2) then (except (tl, l2))
    else hd::(except (tl, l2))

let rec includeAll (l1, l2) = (* is l1 includes all elements of l2? i.e. l2 ⊂ l1 *)
  match l2 with
  | [] -> true
  | hd::tl ->
    if (List.mem hd l1) then includeAll (l1, tl)
    else false

let rec cond2giftList (con, allList) =
  match con with
  | Items (lst) -> lst
  | Same (ide) -> giftOf (ide, allList)
  | Common (con1, con2) -> common (cond2giftList (con1, allList), cond2giftList (con2, allList))
  | Except (con1, lst) -> except (cond2giftList (con1, allList), lst)

let rec satisfyCond (ide, reqCond, allList) =
  let giftList = giftOf (ide, allList) in
  let reqGiftList = cond2giftList (reqCond, allList) in
  includeAll (giftList, reqGiftList)

let rec satisfy (ide, condList, allList) =
  match condList with
  | [] -> true
  | hd::tl -> satisfyCond (ide, hd, allList) && satisfy (ide, tl, allList)

let rec satisfyAll (reqList, allList) =
  satisfy (A, List.nth reqList 0, allList)
  && satisfy (B, List.nth reqList 1, allList)
  && satisfy (C, List.nth reqList 2, allList)
  && satisfy (D, List.nth reqList 3, allList)
  && satisfy (E, List.nth reqList 4, allList)

let reqList = [(A, [Items([1;2]); Common(Same(B), Same(C))]); (B, [Common(Same(C), Items([2;3]))]); (C, [Items([1]); Except(Same(A), [3])])]
let allList = [(A, [1;2]); (B, [2]); (C, [1;2]); (D, []); (E, [])]
(* let shoppingList reqList = *)