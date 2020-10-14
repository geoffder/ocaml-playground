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

let stdout_overwrite_example () =
  for i = 1 to 100 do
    printf "\r%i" i;
    Out_channel.flush stdout;
    Unix.sleep 1
  done
