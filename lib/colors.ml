type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | None

let color_from_string fg =
  match fg with
  | "black" -> Black
  | "red" -> Red
  | "green" -> Green
  | "yellow" -> Yellow
  | "blue" -> Blue
  | "magenta" -> Magenta
  | "cyan" -> Cyan
  | "white" -> White
  | _ -> None

let foreground fg =
  match fg with
  | Black -> "30"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Blue -> "34"
  | Magenta -> "35"
  | Cyan -> "36"
  | White -> "37"
  | None -> ""

let background bg =
  match bg with
  | Black -> "40"
  | Red -> "41"
  | Green -> "42"
  | Yellow -> "43"
  | Blue -> "44"
  | Magenta -> "45"
  | Cyan -> "46"
  | White -> "47"
  | None -> ""

let escapePrefix = "\027["
let reset = escapePrefix ^ "0m"
let makeColor fg bg = escapePrefix ^ Utils.interspersel ";" [ fg; bg ] ^ "m"
let makeforeground fg = escapePrefix ^ fg ^ "m"

let enclose s col =
  let fg = foreground col in
  makeforeground fg ^ s ^ reset
