type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Norway | Sweden | England
| Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let string_of_team = function
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

let rec parenize t =
  match t with
  LEAF l -> string_of_team l
  | NODE (a, b) -> "(" ^ parenize a ^ " " ^ parenize b ^ ")"

let str = parenize (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil))
let _ = print_endline str