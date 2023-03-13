open Printf

let rec sigma a b f =
  if (a <= b) then f a + sigma (a + 1) b f
  else 0

let a = sigma 1 100 (fun x -> x * x)
let _ = printf("%d\n") a