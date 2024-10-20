(** utilities. *)

(* Reimplementing my favorite haskell. stuff. *)

(** The first element of an Array. *)
let car x = x.(0)

(** All but the first element of an Array. *)
let cdr x = Array.sub x 1 (Array.length x - 1)

(* Reimplementing my favorite lisp. stuff. *)

(** car for lists. *)
let head l = match l with [] -> failwith "no first element" | x :: _ -> x

(** cdr for lists. *)
let tail l =
  match l with [] -> failwith "no first element, so no tail." | _ :: xs -> xs

(** Fold array left but use the first element as accumulator. *)
let foldal1 f x = Array.fold_left f (car x) (cdr x)

(** Fold list left but use the first element as accumulator. *)
let foldll1 f x = List.fold_left f (head x) (tail x)

(** Join Array of strings with joiner string. *)
let intersperse joiner = foldal1 (fun acc s -> acc ^ joiner ^ s)

(** Join list of strings with joiner string. *)
let interspersel joiner = foldll1 (fun acc s -> acc ^ joiner ^ s)

(** the usual zip. stops at the shorter list. *)
let rec _zip l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | _x :: _xs, [] -> []
  | [], _y :: _ys -> []
  | x :: xs, y :: ys -> (x, y) :: _zip xs ys

(** applies function `f` to pairs taken from 2 lists. stops at the shorter one. *)
let rec zipwith f l1 l2 =
  match (l1, l2) with
  | [], [] -> ()
  | _x :: _xs, [] -> ()
  | [], _y :: _ys -> ()
  | x :: xs, y :: ys ->
      f x y;
      zipwith f xs ys

(** Create linearly spaced list of floats. *)
let linspace start stop n =
  let step = (stop -. start) /. float_of_int (n - 1) in
  List.init n (fun i -> start +. (float_of_int i *. step))
