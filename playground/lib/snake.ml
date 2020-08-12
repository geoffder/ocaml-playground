open Base

(* Basic linked list type. Mix of tail-recursive and non-tail recursive
 * functions for operating on on them. Purely for practice implementing
 * this familiar data structure from """scratch""" in OCaml. *)

type 'a t =
  | Tip
  | Snek of 'a * 'a t

let empty = Tip

let return a = Snek (a, Tip)

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

(* Tail-recursive reversal. *)
let rev ss =
  let rec loop old new_snek =
    match old with
    | Tip -> new_snek
    | Snek (head, tail) -> grow head new_snek |> loop tail in
  loop ss Tip

let rec map ~f = function
  | Tip -> Tip
  | Snek (head, tail) -> f head @$ map ~f tail

(* Tail-recursive version of map that leaves order reversed. *)
let rev_map ~f ss =
  let rec loop a new_snek =
    match a with
    | Tip -> new_snek
    | Snek (head, tail) -> f head @$ new_snek |> loop tail in
  loop ss Tip

let rec fold ~init ~f = function
  | Tip -> init
  | Snek (head, tail) -> fold ~init:(f init head) ~f tail

(* Reverse first snake, then prepend each element to the second. *)
let append s1 s2 =
  let rec loop acc = function
    | Tip -> acc
    | Snek (h, t) -> loop (h @$ acc) t in
  loop s2 (rev s1)

(* Recursively append second snake to tail of the first. *)
let rec append_nontailrec s1 s2 =
  match s1 with
  | Tip -> s2
  | Snek (h, t) -> h @$ (append_nontailrec t s2)

(* Concatenate a snake of snakes. *)
let join ss =
  let rec loop acc = function
    | Tip -> acc
    | Snek (h, t) -> loop (append acc h) t in
  loop Tip ss

(* Concatenate a snake of snakes while applying f to each element. *)
let join_map ~f ss =
  let rec append_map acc = function
    | Tip -> acc
    | Snek (h, t) -> append_map (f h @$ acc) t in
  let rec loop acc = function
    | Tip -> acc
    | Snek (h, t) -> loop (append_map acc (rev h)) t in
  loop Tip (rev ss)

(* Not as efficient as messy version above. This way takes a mapping function
 * (that operates on snakes, 'a t -> 'b t) rather than a function yet to be
 * mapped (e.g. 'a -> 'b). *)
let bind ~f ss = join ss |> f

let ( >>= ) ss f = bind ~f ss

(* Get element at the given index. Supports negative indexing from end. *)
let get_at ~idx ss =
  let idx, ss = if idx < 0 then (-idx), rev ss else idx, ss in
  let rec loop i = function
    | Tip -> None
    | Snek (h, t) -> if i = idx then Some h else loop (i + 1) t in
  loop 0 ss

(* First element where the predicate f is true. *)
let rec find ~f = function
  | Tip -> None
  | Snek (h, t) -> if f h then Some h else find ~f t

let rec contains ~ele = function
  | Tip -> false
  | Snek (h, t) -> if h = ele then true else contains ~ele t

(* (Maybe) Position of first occurence of matching element. *)
let idx_of ~ele ss =
  let rec loop i = function
    | Tip -> None
    | Snek (h, t) -> if h = ele then Some i else loop (i + 1) t in
  loop 0 ss

(* Perform action f on each element. *)
let rec iter ~f = function
  | Tip -> ()
  | Snek (head, tail) -> f head; iter ~f tail

let filter ~f ss =
  let rec loop acc = function
    | Tip -> acc
    | Snek (h, t) -> if f h then loop (h @$ acc) t else loop acc t in
  loop Tip (rev ss)

(* Last "; " is hackily dropped at the end for simplicity. *)
let pp_string ~stringer ss =
  let f str a = str ^ stringer a ^ "; " in
  ">~~{" ^ String.drop_suffix (fold ~init:"" ~f ss) 2 ^ "}>>>"

module type Stringable = sig
  type t
  val to_string : t -> string
end

(* Same output as pp_string, but takes a module corresponding to the type held
 * in the Snake as a source for the appropriate to_string function. First-class
 * module use similar to how Base.Map gets the necessary comparators. Strings
 * will be printed without quotes with this scheme though. *)
let pp_string' (type a) (module M : Stringable with type t = a) (ss : a t) =
  let f str ele = str ^ M.to_string ele ^ "; " in
  ">~~{" ^ String.drop_suffix (fold ~init:"" ~f ss) 2 ^ "}>>>"
