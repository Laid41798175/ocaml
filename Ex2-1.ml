let rec iter (n, f) =
  if n = 0 then (fun x -> x)
  else fun x -> f (iter ((n - 1), f) x)

let f = fun x -> x + 2

let a = (iter (10, f) 0)
let b = (iter (5, f) 0)
let c = (iter (1, f) 0)
let d = (iter (0, f) 0)

let _ = print_endline(string_of_int(a))
let _ = print_endline(string_of_int(b))
let _ = print_endline(string_of_int(c))
let _ = print_endline(string_of_int(d))