(*
  PL HW7 cps.ml (Exn_skel)
  SNUCSE 18 OH, JINSU
  2018-19857
*)

(*
 * SNU 4190.310 Programming Languages 
 * Homework "Continuation Passing Style" Skeleton
 *)

open Xexp

let count = ref 0

let new_name () = 
  let _ = count := !count + 1 in
  "x_" ^ (string_of_int !count)

let rec alpha_conv exp subst = 
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | Fnr (f, x, e) -> 
    let x' = new_name () in
    let f' = new_name () in
    let subst' = (f, f') :: (x, x') :: subst in
    Fnr (f', x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | Ifp (e1, e2, e3) -> 
    Ifp (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Add (e1, e2) -> Add (alpha_conv e1 subst, alpha_conv e2 subst)
  | Pair (e1, e2) -> Pair (alpha_conv e1 subst, alpha_conv e2 subst)
  | Fst e -> Fst (alpha_conv e subst)
  | Snd e -> Snd (alpha_conv e subst)
  | Raise e -> Raise (alpha_conv e subst)
  | Handle (e1, x, e2) ->
    let x' = new_name () in
    let subst' = (x, x') :: subst in
    Handle (alpha_conv e1 subst, x', alpha_conv e2 subst')

(* TODO : Complete this function *)
let rec xcps' exp = 
  let k_h = new_name () in
  match exp with
  (* Constant expressions *)
  (* use a Fst (Var k_h) instead of Var k_h *)
  (* Constant expressions cannot raise an error *)
  | Num n -> Fn (k_h, App (Fst (Var k_h), Num n))
  | Var x -> Fn (k_h, App (Fst (Var k_h), Var x))
  | Fn (x, e) ->
    Fn (k_h, App (Fst (Var k_h), Fn (x, xcps' e)))
  | Fnr (f, x, e) ->
    Fn (k_h, App (Fst (Var k_h), Fnr (f, x, xcps' e)))
  (* Non constant expressions *)
  (* whenever there is a cps, make a pair (next cps Fn, Snd (Var k_h)) *)
  (* whenever access the raw k_h, use a Fst (Var k_h) *)
  | App (e1, e2) ->
    (* cps e1 -> if good then -> pass to v1 -> cps e2 -> if good then -> pass to v2 -> App (v1 v2) -> pass to k_h
                         else -> return Snd (Var k_h)            else -> return Snd (Var k_h) *)
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k_h, App (xcps' e1,
      Pair (
        Fn (v1, App (xcps' e2,
          Pair (
            Fn (v2, App (App (Var v1, Var v2), Var k_h))
            ,
            Snd (Var k_h)
        )))
        ,
        Snd (Var k_h)
      )
    ))
  | Ifp (e1, e2, e3) ->
    let v1 = new_name () in
    let v2 = new_name () in
    let v3 = new_name () in
    Fn (k_h, App (xcps' e1,
      Pair (
        Fn (v1,
          Ifp (
            Var v1
            ,
            App (xcps' e2,
              Pair (
                Fn (v2, App (Fst (Var k_h), Var v2))
                ,
                Snd (Var k_h)
              )
            )
            ,
            App (xcps' e3,
              Pair (
                Fn (v3, App (Fst (Var k_h), Var v3))
                ,
                Snd (Var k_h)
              )
            )
          )
        )
        ,
        Snd (Var k_h)
      )
    ))
  | Add (e1, e2) ->
    (* cps e1 -> if good then -> pass to v1 -> cps e2 -> if good then -> pass to v2 -> Add (v1 + v2) -> pass to k_h
                         else -> return Snd (Var k_h)            else -> return Snd (Var k_h) *)
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k_h, App (xcps' e1, 
      Pair (
        Fn (v1, App (xcps' e2, 
          Pair (
            Fn (v2, App (Fst (Var k_h), Add (Var v1, Var v2)))
            ,
            Snd (Var k_h)
          )
        ))
        ,
        Snd (Var k_h)
      )
    ))
  | Pair (e1, e2) ->
    (* cps e1 -> if good then -> pass to v1 -> cps e2 -> if good then -> pass to v2 -> make Pair (v1, v2) -> pass to k_h
                         else -> return Snd (Var k_h)            else -> return Snd (Var k_h) *)
    let v1 = new_name () in
    let v2 = new_name () in
    Fn (k_h, App (xcps' e1,
      Pair (
        Fn (v1, App (xcps' e2,
          Pair (
            Fn (v2, App (Var k_h,
              Pair (
                Var v1
                ,
                Var v2
              )
            ))
            ,
            Snd (Var k_h)
          )
        ))
        ,
        Snd (Var k_h)
      )
    ))
  | Fst e ->
    (* cps e -> if good then -> pass to v -> get v.Fst -> pass to k_h
                        else -> return Snd (Var k_h) *)
    let v = new_name() in
    Fn (k_h, App (xcps' e,
      Pair (
        Fn (v, App (Var k_h, Fst (Var v)))
        ,
        Snd (Var k_h)
      )
    ))
  | Snd e ->
    (* cps e -> if good then -> pass to v -> get v.Snd -> pass to k_h
                        else -> return Snd (Var k_h) *)
    let v = new_name() in
    Fn (k_h, App (xcps' e,
      Pair (
        Fn (v, App (Var k_h, Snd (Var v)))
        ,
        Snd (Var k_h)
      )
    ))
  | Raise e ->
    Fn (k_h, App (xcps' e,
      Pair (
        Snd (Var k_h)
        ,
        Num 2023 (* this is logically unreachable *)
      )
    ))
  | Handle (e1, x, e2) ->
    (* cps e1 -> if good then -> pass to k_h
                         else -> cps e2 -> pass to x *)
    Fn (k_h, App (xcps' e1,
      Pair (
        Num 2023
        ,
        Fn (x, App (xcps' e2, Var k_h))
      )
    ))

let xcps exp = xcps' (alpha_conv exp [])
