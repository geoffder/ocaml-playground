open Core

let time_thunk f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Unix.gettimeofday () -. t |> Printf.printf "Execution time: %f seconds\n";
  res

let time_thunk_n_times ~n f =
  let t = Unix.gettimeofday () in
  let () = Fn.apply_n_times ~n f () in
  Unix.gettimeofday () -. t
  |> Printf.printf "Execution time of %i repeats: %f seconds\n" n

let get_time n f =
  Gc.compact ();
  let t = Unix.gettimeofday () in
  for _i = 1 to n do
    ignore (f ())
  done;
  let t2 = Unix.gettimeofday () in
  Gc.compact ();
  t2 -. t

(* NOTE: "%!" is equivalent ot calling `Out_channel.flush stdout`. It does
 * not take any additional arguments. *)
let stdout_overwrite_example () =
  for i = 1 to 100 do
    printf "\r%i%!" i;
    Unix.sleep 1
  done
