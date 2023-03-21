type crazy3 = NIL
  | ZERO of crazy3
  | ONE of crazy3
  | MONE of crazy3
  | TWO of crazy3
  | MTWO of crazy3

let rec crazy3add (c1, c2) =
  match (c1, c2) with
    (NIL, NIL) -> NIL
    | (NIL, c) -> c
    | (c, NIL) -> c
    | (ZERO c10, ZERO c20) -> ZERO (crazy3add (c10, c20))
    | (ZERO c10, ONE c20) -> ONE (crazy3add (c10, c20))
    | (ZERO c10, MONE c20) -> MONE (crazy3add (c10, c20))
    | (ZERO c10, TWO c20) -> TWO (crazy3add (c10, c20))
    | (ZERO c10, MTWO c20) -> MTWO (crazy3add (c10, c20))
    | (ONE c10, ZERO c20) -> ONE (crazy3add (c10, c20))
    | (ONE c10, ONE c20) -> TWO (crazy3add (c10, c20))
    | (ONE c10, MONE c20) -> ZERO (crazy3add (c10, c20))
    | (ONE c10, TWO c20) -> ZERO (crazy3add ((crazy3add (c10, c20)), (ONE NIL))) (* Carry *)
    | (ONE c10, MTWO c20) -> MONE (crazy3add (c10, c20))
    | (MONE c10, ZERO c20) -> MONE (crazy3add (c10, c20))
    | (MONE c10, ONE c20) -> ZERO (crazy3add (c10, c20))
    | (MONE c10, MONE c20) -> MTWO (crazy3add (c10, c20))
    | (MONE c10, TWO c20) -> ONE (crazy3add (c10, c20))
    | (MONE c10, MTWO c20) -> ZERO (crazy3add ((crazy3add (c10, c20)), (MONE NIL))) (* Carry *) 
    | (TWO c10, ZERO c20) -> TWO (crazy3add (c10, c20))
    | (TWO c10, ONE c20) -> ZERO (crazy3add ((crazy3add (c10, c20)), (ONE NIL))) (* Carry *)
    | (TWO c10, MONE c20) -> ONE (crazy3add (c10, c20))
    | (TWO c10, TWO c20) -> ONE (crazy3add ((crazy3add (c10, c20)), (ONE NIL))) (* Carry *)
    | (TWO c10, MTWO c20) -> ZERO (crazy3add (c10, c20))
    | (MTWO c10, ZERO c20) -> MTWO (crazy3add (c10, c20))
    | (MTWO c10, ONE c20) -> MONE (crazy3add (c10, c20))
    | (MTWO c10, MONE c20) -> MONE (crazy3add ((crazy3add (c10, c20)), (MONE NIL))) (* Carry *) 
    | (MTWO c10, TWO c20) -> ZERO (crazy3add (c10, c20))
    | (MTWO c10, MTWO c20) -> MONE (crazy3add ((crazy3add (c10, c20)), (MONE NIL))) (* Carry *)