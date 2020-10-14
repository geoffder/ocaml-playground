open Core

let time_thunk f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Unix.gettimeofday () -. t
  |> Printf.printf "Execution time: %f seconds\n";
  res

let time_thunk_n_times ~n f =
  let t = Unix.gettimeofday () in
  let () = Fn.apply_n_times ~n f () in
  Unix.gettimeofday () -. t
  |> Printf.printf "Execution time of %i repeats: %f seconds\n" n
