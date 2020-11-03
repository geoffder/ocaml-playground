open Core
open Async
open AgentSQ

module Crunch = struct
  type msg =
    | AddSum of int list
    | AddSumN of int * (int list)
    | Show
    | Fetch of int t
    | Stop

  let sum = List.fold ~init:0 ~f:( + )

  let body _mailbox state = function
    | AddSum l       -> State (state + sum l)
    | AddSumN (n, l) -> State (Fn.apply_n_times ~n (fun s -> s + sum l) state)
    | Show           -> printf "state = %d\n" state; State state
    | Stop           -> Stop
    | Fetch chan     -> reply chan state; State state

  let run () =
    let big_list = List.range 0 1000 in
    let mailbox = create ~init:0 body in begin
      print_endline "Pre print.";
      post mailbox (AddSumN (1000000, big_list));
      print_endline "Mid print.";
      Fn.apply_n_times ~n:1000000 (fun s -> s + sum big_list) 0
      |> printf "main sum: %i\n";
      post mailbox Show;
      post_and_reply_exn mailbox (fun c -> Fetch c) >>= fun reply ->
      printf "Fetched state = %i\n" reply;
      post mailbox Stop;
      after (Time.Span.of_sec 5.0)
    end |> Fn.flip upon @@ (fun () -> Shutdown.shutdown 0);
    never_returns (Scheduler.go ())
end
