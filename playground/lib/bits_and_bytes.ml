open Core

let bigend_bytes_to_int bytes =
  Bytes.to_list bytes
  |> List.rev
  |> List.foldi ~init:0
    ~f:(fun i acc c -> acc + (Char.to_int c) * (Int.pow 256 i))

let bigend_bytes_to_int64 bytes =
  let f i acc c =
    Int64.(acc + (of_int @@ Char.to_int c) * (Int64.pow (of_int 256) (of_int i)))
  in
  Bytes.to_list bytes
  |> List.rev
  |> List.foldi ~init:(Int64.of_int 0) ~f

let bin8_of_int_exn d =
  if d < 0 || d > 255
  then failwith "Must be unsigned 8bit int."
  else
    let rec aux acc = function
      | 0 -> acc
      | d -> aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    let bin = String.concat (aux [] d) in
    let pad = String.init ~f:(fun _ -> '0') (8 - String.length bin) in
    pad ^ bin

let int_of_bin_exn s =
  let char_to_bit = function
    | '0' -> 0
    | '1' -> 1
    | _   -> failwith "Bitstring must only consist of 0s and 1s."
  in
  String.lstrip ~drop:(Char.equal '0') s
  |> String.rev
  |> String.foldi ~init:0
    ~f:(fun i sum c -> sum + (char_to_bit c * (Int.pow 2 i)))

let int_to_bigend_bytes ~len i =
  let bs = Bytes.create len in
  let rec loop n rem =
    if n >= 0 then
      let b = Int.pow 256 n in
      Bytes.set bs (len - n - 1) (Char.of_int_exn (rem / b));
      loop (n - 1) (rem mod b)
    else ()
  in
  loop (len - 1) i;
  bs
