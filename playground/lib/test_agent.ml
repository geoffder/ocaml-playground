open Core
open Async
open Agent

module Basics = struct
  type msg =
    | Add of int
    | Show
    | BindUp of float
    | SlowShow of float
    | Fetch of int t
    | Stop

  let body _mailbox state = function
    | Add i -> State (state + i)
    | Show  -> printf "state = %d\n" state; State state
    | BindUp secs -> Async begin
        return (print_endline "(bind) just a sec...") >>= fun () ->
        after (Time.Span.of_sec secs) >>= fun () ->
        return state
      end
    | SlowShow secs -> begin
        after (Time.Span.of_sec secs) >>= fun () ->
        return (printf "(slow) state = %d\n" state)
      end |> don't_wait_for; State state
    | Stop  -> Stop
    | Fetch chan -> reply chan state; State state

  let run () =
    let mailbox = create~init:0 body in begin
      post mailbox (Add 1)                          >>= fun () ->
      post mailbox (Add 1)                          >>= fun () ->
      post mailbox Show                             >>= fun () ->
      post mailbox (BindUp 3.0)                     >>= fun () ->
      post mailbox (Add 10)                         >>= fun () ->
      post mailbox Show                             >>= fun () ->
      post mailbox (Add 1)                          >>= fun () ->
      post mailbox (Add 1)                          >>= fun () ->
      post mailbox (Add 1)                          >>= fun () ->
      post mailbox (Add 1)                          >>= fun () ->
      post mailbox (SlowShow 3.0)                   >>= fun () ->
      post mailbox (SlowShow 3.0)                   >>= fun () ->
      post mailbox (SlowShow 3.0)                   >>= fun () ->
      post_and_reply_exn mailbox (fun c -> Fetch c)   >>= fun reply ->
      return (printf "Fetched state = %i\n" reply)  >>= fun () ->
      post mailbox Stop                             >>= fun () ->
      after (Time.Span.of_sec 5.0)
    end |> Fn.flip upon @@ (fun () -> Shutdown.shutdown 0);
    never_returns (Scheduler.go ())
end

module PingPong = struct
  type msg =
    | SendPing of msg Agent.t
    | Ping of msg Agent.t
    | Pong of string
    | NameChange of string
    | Kill

  let body mailslot name = function
    | SendPing target -> reply target (Ping mailslot);                 State name
    | Ping sender     -> reply sender (Pong name);                     State name
    | Pong s          -> printf "%s: Pong recieved from %s!\n" name s; State name
    | NameChange n    -> printf "%s's name is now %s.\n" name n;       State n
    | Kill            -> Stop

  let run () =
    let agent_a = create ~init:"Joe" body in
    let agent_b = create ~init:"Bob" body in
    begin
      post agent_a (SendPing agent_b)   >>= fun () ->
      post agent_b (SendPing agent_a)   >>= fun () ->
      post agent_a (SendPing agent_b)   >>= fun () ->
      post agent_a (NameChange "Steve") >>= fun () ->
      post agent_b (SendPing agent_a)   >>= fun () ->
      post agent_a Kill                 >>= fun () ->
      post agent_b Kill                 >>= fun () ->
      after (Time.Span.of_sec 1.0)
    end |> Fn.flip upon @@ (fun () -> Shutdown.shutdown 0);
    never_returns (Scheduler.go ())
end

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
    let big_list = List.range 0 10000000 in
    let mailbox = create_in_thread ~init:0 body in begin
      return (print_endline "Pre print.")           >>= fun () ->
      after (Time.Span.of_sec 1.0)                  >>= fun () ->
      post mailbox (AddSumN (100, big_list))        >>= fun () ->
      return (print_endline "Regular print.")       >>= fun () ->
      post mailbox (AddSumN (100, big_list))        >>= fun () ->
      post mailbox Show                             >>= fun () ->
      post_and_reply_exn mailbox (fun c -> Fetch c)   >>= fun reply ->
      return (printf "Fetched state = %i\n" reply)  >>= fun () ->
      post mailbox Stop                             >>= fun () ->
      after (Time.Span.of_sec 5.0)
    end |> Fn.flip upon @@ (fun () -> Shutdown.shutdown 0);
    never_returns (Scheduler.go ())
end
