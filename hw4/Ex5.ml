(* PL HW4 Ex5.ml
   SNUCSE 18 OH, JINSU
   2018-19857
*)

type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key

type map = End of treasure
| Branch of map * map
| Guide of string * map

exception IMPOSSIBLE

(* module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

type env = (string, env_entry) Env.t
and  env_entry = Key

let emptyEnv = Env.empty

let rec implyKey : map * env -> key * env =
  fun (map0, myEnv) ->
    match map0 with
    | End StarBox -> (Bar, myEnv)
    | End NameBox str -> 
        let newEnv = Env.bind myEnv str Key in
        (Bar, newEnv)
    | Guide (str, map1) ->
        let (key1, _) = Env.lookup myEnv str in
        let (key2, _) = implyKey map1 myEnv in
        Node (key1, key2)
    | Branch (map1, map2) ->
        let key1 = implyKey map1 myEnv in
        match key1 with
        | Bar -> raise IMPOSSIBLE
        | Node (key1a, key1b) ->
          let key2 = implyKey map2 myEnv in
          if (key1a = key2) then
            key1b
          else raise IMPOSSIBLE *)

let rec getReady : map -> key list =
  fun _ ->
    [Bar]