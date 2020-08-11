open Base

(* Basic linked list type. *)

type 'a t =
  | Tip
  | Snek of 'a * 'a t

let grow a ss = Snek (a, ss)

let ( @$ ) a ss = Snek (a, ss)

let rec of_list = function
  | [] -> Tip
  | hd :: tl -> hd @$ of_list tl

let rev ss =
  let rec loop old new_snek =
    match old with
    | Tip -> new_snek
    | Snek (ele, rest) -> grow ele new_snek |> loop rest in
  loop ss Tip

let rec map ~f = function
  | Tip -> Tip
  | Snek (ele, rest) -> f ele @$ map ~f rest

let map_rev ~f ss =
  let rec loop a new_snek =
    match a with
    | Tip -> new_snek
    | Snek (ele, rest) -> f ele @$ new_snek |> loop rest in
  loop ss Tip
