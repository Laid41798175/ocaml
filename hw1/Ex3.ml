type crazy3 = NIL
  | ZERO of crazy3
  | ONE of crazy3
  | MONE of crazy3
  | TWO of crazy3
  | MTWO of crazy3

let rec crazy3val c =
  match c with
  NIL -> 0
  | ZERO c3 -> 3 * crazy3val c3
  | ONE c3 -> 1 + 3 * crazy3val c3
  | MONE c3 -> -1 + 3 * crazy3val c3
  | TWO c3 -> 2 + 3 * crazy3val c3
  | MTWO c3 -> -2 + 3 * crazy3val c3