(*
 * SNU 4190.310 Programming Languages 2023 Spring
 *  K- Interpreter Skeleton Code
 *)

(* PL HW4 k.ml
   SNUCSE 18 OH, JINSU
   2018-19857
*)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error ("Unbound: " ^ x))

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error ("Unbound: " ^ f))

  (* return (list of value calculated sequencely, final memory) *)
  let rec call_value_list mem env e_list =
    match e_list with
    | [] -> ([], mem)
    | hd::tl ->
      let (v, me1) = eval mem env hd in
      let (v_list, me2) = call_value_list me1 env tl in
      (v::v_list, me2)
  
  (* continuous env bind, return (final memory, final env) *)
  and call_env_bind mem env x_list v_list = 
    match (x_list, v_list) with
    | ([], []) -> (mem, env)
    | (xh::xt, vh::vt) ->
      let (l, me1) = Mem.alloc mem in
      let en1 = Env.bind env xh (Addr l) in
      let me2 = Mem.store me1 l vh in
      call_env_bind me2 en1 xt vt
    | _ -> (* x_list.length != v_list.length *)
      raise (Error "InvalidArg")
  
  (* continuous env bind, referencing en2. return final env *)
  and call_r_env_bind en1 en2 x_list y_list = 
    match (x_list, y_list) with
    | ([], []) -> en1
    | (xh::xt, yh::yt) ->
      let l = lookup_env_loc en2 yh in
      let en3 = Env.bind en1 xh (Addr l) in
      call_r_env_bind en3 en2 xt yt
    | _ -> (* x_list.length != y_list.length *)
      raise (Error "InvalidArg")

  (* split id * exp list to (id list, exp list) *)
  and split_list x_e_list =
    match (x_e_list) with
    | [] -> ([], [])
    | (xh, eh)::tl ->
      let (x_list, e_list) = split_list tl in
      (xh::x_list, eh::e_list)
  
  (* when record_bind is first called, env must be Empty!! *)
  and record_bind mem env x_list v_list =
    match (x_list, v_list) with
    | ([], []) -> (mem, env)
    | (xh::xt, vh::vt) ->
      let (l, me1) = Mem.alloc mem in
      let me2 = Mem.store me1 l vh in
      let en1 = Env.bind env xh (Addr l) in
      record_bind me2 en1 xt vt
    | _ -> (* x_list.length != v_list.length *)
      (* Logically, this block is unreachable because we asserted that list's length is same in call_value_list already *)
      raise (Error "InvalidArg")

  and eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | NUM i -> (Num i, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR id -> 
      let l = lookup_env_loc env id in
      let v = Mem.load mem l in
      (v, mem)
    | ADD (e1, e2) ->
      let (v1, me1) = eval mem env e1 in
      let (v2, me2) = eval me1 env e2 in
      (Num(value_int v1 + value_int v2), me2)
    | SUB (e1, e2) ->
      let (v1, me1) = eval mem env e1 in
      let (v2, me2) = eval me1 env e2 in
      (Num(value_int v1 - value_int v2), me2)
    | MUL (e1, e2) ->
      let (v1, me1) = eval mem env e1 in
      let (v2, me2) = eval me1 env e2 in
      (Num(value_int v1 * value_int v2), me2)
    | DIV (e1, e2) ->
      let (v1, me1) = eval mem env e1 in
      let (v2, me2) = eval me1 env e2 in
      (Num(value_int v1 / value_int v2), me2)
    | EQUAL (e1, e2) -> begin
      let (v1, me1) = eval mem env e1 in
      let (v2, me2) = eval me1 env e2 in
      try
        (Bool(v1 = v2), me2)
      with _ ->
        (Bool(false), me2)
      end
    | LESS (e1, e2) ->
      let (v1, me1) = eval mem env e1 in
      let (v2, me2) = eval me1 env e2 in
      (Bool(value_int v1 < value_int v2), me2)
    | NOT e1 ->
      let (v1, me1) = eval mem env e1 in
      if (value_bool v1) then (Bool(false), mem)
      else (Bool(true), mem)
    | SEQ (e1, e2) ->
      let (v1, me1) = eval mem env e1 in
      eval me1 env e2
    | IF (e1, e2, e3) ->
      let (v1, me1) = eval mem env e1 in
      if (value_bool v1) then (eval me1 env e2)
      else (eval me1 env e3)
    | WHILE (e1, e2) ->
      let (v1, me1) = eval mem env e1 in
      if (value_bool v1) then
        let (v2, me2) = (eval me1 env e2) in
        eval me2 env (WHILE (e1, e2))
      else (Unit, me1)
    | LETF (f, args, e1, e2) ->
      let pr = Proc (args, e1, env) in
      eval mem (Env.bind env f pr) e2
    | CALLV (f, e_list) ->
      let (x_list, exp, en1) = lookup_env_proc env f in
      let pr = Proc (x_list, exp, en1) in
      let (v_list, me1) = call_value_list mem env e_list in
      let (me2, en2) = call_env_bind me1 en1 x_list v_list in
      let en3 = Env.bind en2 f pr in (* finally, bind f to en2 *)
      eval me2 en3 exp      
    | CALLR (f, y_list) ->      
      let (x_list, exp, en1) = lookup_env_proc env f in
      let en2 = call_r_env_bind en1 env x_list y_list in
      eval mem en2 exp
    | RECORD (x_e_list) ->
      let (x_list, e_list) = split_list x_e_list in
      let (v_list, me1) = call_value_list mem env e_list in
      let en1 = Env.empty in
      let (me2, en2) = record_bind me1 en1 x_list v_list in
      (Record (lookup_env_loc en2), me2)
    | FIELD (e1, x) ->
      let (en1, me1) = eval mem env e1 in
      let l = 
        (match en1 with
      | Record r -> r x
      | _ -> raise (Error "TypeError : not record"))
      in
      let v = Mem.load me1 l in
      (v, me1)
    | ASSIGNF (e1, x, e2) ->
      let (en1, me1) = eval mem env e1 in
      let (v, me2) = eval me1 env e2 in
      let l = 
        (match en1 with
      | Record r -> r x
      | _ -> raise (Error "TypeError : not record"))
      in
      let me3 = Mem.store me2 l v in
      (v, me3)

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
