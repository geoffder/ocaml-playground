open Base

(* Basic linked list type. *)

type 'a t =
  | Tip
  | Snek of 'a * 'a t

let empty = Tip

let is_empty = function
  | Tip -> true
  | _   -> false

let grow a ss = Snek (a, ss)

let ( @$ ) a ss = Snek (a, ss)

let hd = function
  | Tip -> None
  | Snek (a, _) -> Some a

let tl = function
  | Tip -> None
  | Snek (_, a) -> Some a

let rec of_list = function
  | [] -> Tip
  | h :: t -> h @$ of_list t

let rec to_list = function
  | Tip -> []
  | Snek (head, tail) -> head :: to_list tail

let rev ss =
  let rec loop old new_snek =
    match old with
    | Tip -> new_snek
    | Snek (head, tail) -> grow head new_snek |> loop tail in
  loop ss Tip

let rec map ~f = function
  | Tip -> Tip
  | Snek (head, tail) -> f head @$ map ~f tail

let map_rev ~f ss =
  let rec loop a new_snek =
    match a with
    | Tip -> new_snek
    | Snek (head, tail) -> f head @$ new_snek |> loop tail in
  loop ss Tip

let rec fold ~init ~f = function
  | Tip -> init
  | Snek (head, tail) -> fold ~init:(f init head) ~f tail

let pp_string ~stringer ss =
  let f str a = str ^ stringer a ^ "; " in
  "~~<" ^ (fold ~init:"" ~f ss) ^ ">~~"
