open Core
open Async

type 'a t      = 'a Pipe.Writer.t
type 'a inbox  = 'a Pipe.Reader.t
type 'b answer = State of 'b | Async of 'b Deferred.t | Stop

let read        : 'a inbox -> [> `Eof | `Ok of 'a ] Deferred.t = Pipe.read
let post        : 'a t -> 'a -> unit Deferred.t                = Pipe.write
let close       : 'a t -> unit                                = Pipe.close
let close_inbox : 'a inbox -> unit                            = Pipe.close_read

let ( >> ) f g x = g (f x)

let ( >?= ) answer loop =
  match answer with
  | State s -> loop s
  | Async d -> d >>= loop
  | Stop    -> return ()

let create ?(size=100) ~init body : 'a t =
  let (inbox, mailslot) = Pipe.create () in
  let () = Pipe.set_size_budget inbox size in
  let rec msg_loop state =
    read inbox >>= function
    | `Eof    -> return ()
    | `Ok msg -> body state msg >?= msg_loop in
  let () = upon (msg_loop init) (fun () -> close mailslot) in
  mailslot

let reply channel = post channel >> don't_wait_for

let result_of_timeout = function
  | `Result a -> Result.return a
  | `Timeout  -> Result.fail "Timeout."

let post_and_reply ?(timeout=86400.) t closure =
  let times_up = Time.Span.of_sec timeout |> with_timeout in
  let (inbox, channel) = Pipe.create () in
  let msg = closure channel in
  post t msg >>= fun () ->
  read inbox |> times_up >>| result_of_timeout >>=? function
  | `Eof  -> Deferred.Result.fail "Reply channel unexpectedly closed (`Eof)"
  | `Ok r -> close_inbox inbox; Deferred.Result.return r

(* NOTE: Warning, shouldn't be using block_on_async within async code. *)
let post_and_reply_sync t closure =
  (fun () -> post_and_reply t closure)
  |> Thread_safe.block_on_async
  |> Result.map_error ~f:(Error.of_exn >> Error.to_string_hum)
  |> Result.join

let post_and_reply_exn t closure =
  post_and_reply t closure >>| Result.ok_or_failwith

(* NOTE: Warning, shouldn't be using block_on_async within async code. *)
let post_and_reply_sync_exn t closure =
  (fun () -> post_and_reply_exn t closure) |> Thread_safe.block_on_async_exn

module Test = struct
  type msg =
    | Add of int
    | Show
    | BindUp of float
    | SlowShow of float
    | Fetch of int t
    | Stop

  let body state = function
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
    let mailbox = create ~init:0 body in begin
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
