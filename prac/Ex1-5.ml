open Printf

type crazy2 =
  NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val c2 =
  match c2 with
  NIL -> 0
  | ZERO c -> 2 * crazy2val c
  | ONE c -> (2 * crazy2val c) + 1
  | MONE c -> (2 * crazy2val c) - 1

let rec crazy2add c1 c2 =
  match (c1, c2) with
    (NIL, NIL) -> NIL
    | (NIL, c) -> c
    | (c, NIL) -> c
    | (ZERO c10, ZERO c20) -> ZERO (crazy2add c10 c20)
    | (ZERO c10, ONE c20) -> ONE (crazy2add c10 c20)
    | (ZERO c10, MONE c20) -> MONE (crazy2add c10 c20)
    | (ONE c10, ZERO c20) -> ONE (crazy2add c10 c20)
    | (ONE c10, ONE c20) -> ZERO (crazy2add (crazy2add c10 c20) (ONE NIL)) (* Carry *)
    | (ONE c10, MONE c20) -> ZERO (crazy2add c10 c20)
    | (MONE c10, ZERO c20) -> MONE (crazy2add c10 c20)
    | (MONE c10, ONE c20) -> ZERO (crazy2add c10 c20)
    | (MONE c10, MONE c20) -> ZERO (crazy2add (crazy2add c10 c20) (MONE NIL)) (* Carry *)
  ;;
  
let zero = ZERO NIL
let one = MONE (ONE (ZERO NIL))
let two = ZERO (ONE NIL)
let three = ONE (MONE (ONE (ZERO (ZERO NIL))))
let four = ZERO (ZERO (MONE (ONE NIL)))
let m_five = ONE (MONE (ONE (MONE NIL)))
let m_one = (ONE(ONE(ONE(ONE(MONE NIL)))));;

let _ = printf ("%d\n") (crazy2val zero)
let _ = printf ("%d\n") (crazy2val one)
let _ = printf ("%d\n") (crazy2val two)
let _ = printf ("%d\n") (crazy2val four)
let _ = printf ("%d\n") (crazy2val m_five)
let _ = printf ("%d\n") (crazy2val m_one)

let _ = printf ("%d\n") (crazy2val (crazy2add one two))
let _ = printf ("%d\n") (crazy2val (crazy2add four two))
let _ = printf ("%d\n") (crazy2val (crazy2add m_one two))
let _ = printf ("%d\n") (crazy2val (crazy2add m_five three))
let _ = printf ("%d\n") (crazy2val (crazy2add zero m_five))