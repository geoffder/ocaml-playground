let pascals_triangle n =
  let rec loop i row =
    if i = n
    then []
    else (
      let a = 0 :: row in
      row :: loop (i + 1) (List.map2 ( + ) a (List.rev a)) )
  in
  loop 0 [ 1 ]

let fold2_ragged f init a b =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | h1 :: t1, h2 :: t2 -> aux (f acc h1 h2) t1 t2
    | [], _ | _, []      -> acc
  in
  aux init a b

(* Naive and slow implementation of internal functions found here:
   https://github.com/revarbat/BOSL2/blob/1b470166331d953042e545357433d0234045b6ee/beziers.scad

   Faster cached version of the triangle can be found in the Scad_ml library.
   Just snipped this out for reference since I'm deleting it from over there. *)
let signed_pascals_triangle n =
  let row (j, acc) a b =
    let sign = if j mod 2 = 1 then -1. else 1. in
    j + 1, (sign *. (Float.abs a +. Float.abs b)) :: acc
  in
  let rec loop tri last_row i =
    if i = n
    then tri
    else (
      let _, r = fold2_ragged row (0, []) last_row (List.tl last_row) in
      let row = -1. :: List.rev ((if i mod 2 = 0 then 1. else -1.) :: r) in
      loop (row :: tri) row (i + 1) )
  in
  List.rev @@ loop [ [ -1. ] ] [ -1. ] 0

let bezier_matrix n =
  let tri = List.map Array.of_list (signed_pascals_triangle n) |> Array.of_list
  and m = Array.make_matrix (n + 1) (n + 1) 0. in
  for i = 0 to n do
    let a = tri.(i) in
    let b = tri.(n).(i) in
    for j = 0 to i do
      m.(i).(j) <- a.(j) *. b
    done
  done;
  m
