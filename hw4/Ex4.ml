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

let rec reqOf : id * require list -> cond list =
  fun (id1, reqList) ->
    match reqList with
    | [] -> []
    | hd::tl ->
        match hd with
        | (id2, cl) ->
          if (id2 = id1) then cl
          else reqOf (id1, tl)

let rec giftOf : id * (id * gift list) list -> gift list =
  fun (id1, allList) ->
    match allList with
    | [] -> []
    | hd::tl ->
        match hd with
        | (id2, gl) ->
          if (id2 = id1) then gl
          else giftOf (id1, tl)

let rec common : gift list * gift list -> gift list =
  fun (l1, l2) ->
    match l2 with
    | [] -> []
    | hd::tl ->
      if (List.mem hd l1) then hd::(common (l1, tl))
      else (common (l1, tl))

let rec except : gift list * gift list -> gift list =
  fun (l1, l2) ->
    match l1 with
    | [] -> []
    | hd::tl ->
      if (List.mem hd l2) then (except (tl, l2))
      else hd::(except (tl, l2))

let rec includeAll : gift list * gift list -> bool = 
  fun (l1, l2) -> (* is l1 includes all elements of l2? i.e. l2 ⊂ l1 *)
    match l2 with
    | [] -> true
    | hd::tl ->
      if (List.mem hd l1) then includeAll (l1, tl)
      else false

(* if allList is given, every cond can be converted into gift list *)
let rec cond2giftList : cond * (id * gift list) list -> gift list =
  fun (con, allList) ->
    match con with
    | Items (lst) -> lst
    | Same (ide) -> giftOf (ide, allList)
    | Common (con1, con2) -> common (cond2giftList (con1, allList), cond2giftList (con2, allList))
    | Except (con1, lst) -> except (cond2giftList (con1, allList), lst)

let rec satisfyCond : id * cond * (id * gift list) list -> bool =
  fun (ide, reqCond, allList) ->
    let giftList : gift list = giftOf (ide, allList) in
    let reqGiftList : gift list = cond2giftList (reqCond, allList) in
    includeAll (giftList, reqGiftList)

let rec satisfy : id * cond list * (id * gift list) list -> bool =
  fun (ide, condList, allList) ->
    match condList with
    | [] -> true
    | hd::tl -> satisfyCond (ide, hd, allList) && satisfy (ide, tl, allList)

let rec satisfyAll : require list * (id * gift list) list -> bool =
  fun (reqList, allList) ->
    satisfy (A, reqOf(A, reqList), allList)
    && satisfy (B, reqOf(B, reqList), allList)
    && satisfy (C, reqOf(C, reqList), allList)
    && satisfy (D, reqOf(D, reqList), allList)
    && satisfy (E, reqOf(E, reqList), allList)

let rec merge : gift list * gift list -> gift list =
  fun (newList, orgList) ->
    match newList with
    | [] -> orgList
    | hd::tl ->
        if (List.mem hd orgList) then merge(tl, orgList)
        else merge(tl, hd::orgList)

let rec condCandList : cond * gift list -> gift list =
  fun (con, giftList) ->
    match con with
    | Items gl -> merge(gl, giftList)
    | Same _ -> giftList
    | Common (c1, c2) ->
        let a = condCandList(c1, giftList) in
        condCandList(c2, a)
    | Except (c1, gl) ->
        condCandList(c1, giftList)
      
let rec candList : cond list * gift list -> gift list =
  fun (condList, giftList) ->
    match condList with
    | [] -> giftList
    | hd::tl -> 
        let a = condCandList(hd, giftList) in
        candList (tl, a)

let rec allCandList : require list -> gift list =
  fun reqList ->
    let a = candList(reqOf(A, reqList), []) in
    let b = candList(reqOf(B, reqList), a) in
    let c = candList(reqOf(C, reqList), b) in
    let d = candList(reqOf(D, reqList), c) in
    let e = candList(reqOf(E, reqList), d) in
    List.sort compare e

let rec sublists = function
  | [] -> [[]]
  | hd::tl ->
    let sub = sublists tl in
    let res = sub @ List.map (fun l -> hd::l) sub in
    List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2)) res

let cost : gift list list -> int =
  fun (gll) ->
    List.length (List.nth gll 0) + List.length (List.nth gll 1) + List.length (List.nth gll 2)
    + List.length (List.nth gll 3) + List.length (List.nth gll 4)

let giftListToAllList : gift list list -> (id * gift list) list =
  fun (l) -> [(A, List.nth l 0); (B, List.nth l 1); (C, List.nth l 2); (D, List.nth l 3); (E, List.nth l 4)]

let rec checkAndUpdate : require list * gift list list list * int * (id * gift list) list -> (id * gift list) list =
  fun (reqList, glll, min, exist) ->
    match glll with 
    | [] -> exist
    | hd::tl ->
      let cst = cost hd in
      if (cst < min) then
        let al = giftListToAllList hd in
        if satisfyAll(reqList, al) then
          checkAndUpdate (reqList, tl, cst, al)
        else
          checkAndUpdate (reqList, tl, min, exist)
      else
        checkAndUpdate (reqList, tl, min, exist)
      
let shoppingList : require list -> (id * gift list) list =
  fun (reqList) ->
    let candList = allCandList(reqList) in
    let sl = sublists(candList) in
    let num = List.length sl in
    let glll = ref [] in
    for i1 = 0 to num-1 do
      for i2 = 0 to num-1 do
        for i3 = 0 to num-1 do
          for i4 = 0 to num-1 do
            for i5 = 0 to num-1 do
              let gll = [(List.nth sl i1); (List.nth sl i2); (List.nth sl i3); (List.nth sl i4); (List.nth sl i5)] in
              glll := gll :: !glll
            done;
          done;
        done;
      done;
    done;
    checkAndUpdate(reqList, !glll, 1000, [(A, []); (B, []); (C, []); (D, []); (E, [])])

(* if candList(# of gift appeared in conditions) is greater than or equal to 5, TLE *)