type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument

let rec diff (exp, x) =
  match exp with
  CONST _ -> CONST 0
  | VAR t -> if t = x then CONST 1 else CONST 0
  | POWER (t, i) ->
      if t = x then TIMES[CONST i; POWER(t, i - 1)]
      else CONST 0
  | TIMES exp_list ->
    let len = List.length exp_list in
    if len = 0 then raise InvalidArgument
    else let rec times_diff i results = 
      if i >= len then results
      else let new_result = TIMES (List.mapi (fun index e -> if i = index then diff (e, x) else e) exp_list) in
        times_diff (i + 1) (new_result::results)
    in SUM (times_diff 0 []) 
  | SUM exp_list ->
    match exp_list with
    [] -> raise InvalidArgument
    | _ -> SUM (List.map (fun e -> diff (e, x)) exp_list);;
  ;;
