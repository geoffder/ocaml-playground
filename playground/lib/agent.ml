open Core
open Async

type 'a t = ('a, Pipe.Writer.phantom) Pipe.t

type 'b inbox = ('b, Pipe.Reader.phantom) Pipe.t

type 'b answer = State of 'b | Stop

let ( >> ) f g x = g (f x)

let ( >?= ) answer loop = answer >>= function
  | State s -> loop s
  | Stop    -> return ()

let create ?(size=100) ~init body : 'a t =
  let (inbox, mailslot) = Pipe.create () in
  let () = Pipe.set_size_budget inbox size in
  let rec msg_loop state =
    Pipe.read inbox >>= function
    | `Eof -> return ()
    | `Ok msg -> return (body state msg) >?= msg_loop in
  let () = msg_loop init >>= (fun () -> return (Pipe.close mailslot))
           |> don't_wait_for in
  mailslot

let post : 'a t -> 'a -> unit Deferred.t = Pipe.write

let close : 'a t -> unit = Pipe.close

let result_of_timeout = function
  | `Result a -> Result.return a
  | `Timeout  -> Result.fail "Timeout."

let post_and_reply ?(timeout=86400.) t closure =
  let times_up = Time.Span.of_sec timeout |> with_timeout in
  let (reply, channel) = Pipe.create () in
  let msg = closure channel in
  post t msg >>= fun () ->
  Pipe.read reply |> times_up >>| result_of_timeout >>=? function
  | `Eof -> Deferred.Result.fail "Reply channel unexpectedly closed (`Eof)"
  | `Ok r -> Deferred.Result.return r

let post_and_reply_sync t closure =
  (fun () -> post_and_reply t closure)
  |> Thread_safe.block_on_async
  |> Result.map_error ~f:(Error.of_exn >> Error.to_string_hum)
  |> Result.join

let post_and_reply_exn t closure =
  post_and_reply t closure >>| Result.ok_or_failwith

let post_and_reply_sync_exn t closure =
  (fun () -> post_and_reply_exn t closure) |> Thread_safe.block_on_async_exn

module Test = struct
  type msg = Add of int | Show | Stop | Fetch of int t

  let body state = function
    | Add i -> State (state + i)
    | Show  -> printf "state = %d\n" state; State state
    | Stop  -> Stop
    | Fetch chan -> Pipe.write chan state |> don't_wait_for; State state

  let run () =
    let mailbox = create ~init:0 body in
    post mailbox (Add 1)  >>= fun () ->
    post mailbox (Add 1)  >>= fun () ->
    post mailbox Show     >>= fun () ->
    post mailbox (Add 10) >>= fun () ->
    post mailbox Show     >>= fun () ->
    post mailbox (Add 1)  >>= fun () ->
    post mailbox (Add 1)  >>= fun () ->
    post mailbox (Add 1)  >>= fun () ->
    post mailbox (Add 1)  >>= fun () ->
    post_and_reply_exn mailbox (fun c -> Fetch c) >>= fun reply ->
    return (printf "Fetched state = %i" reply)  >>= fun () ->
    post mailbox Stop |> don't_wait_for;
    never_returns (Scheduler.go ())
end
