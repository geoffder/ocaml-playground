open Core
open Async

type 'a t = ('a, Pipe.Writer.phantom) Pipe.t

type 'b inbox = ('b, Pipe.Reader.phantom) Pipe.t

type 'b answer = State of 'b | Stop

let ( >?= ) answer loop = answer >>= function
  | State s -> loop s
  | Stop    -> return ()

let create ?(size=100) body initial_state : 'a t =
  let (inbox, mailslot) = Pipe.create () in
  let () = Pipe.set_size_budget inbox size in
  let rec msg_loop state =
    Pipe.read inbox >>= function
    | `Eof -> return ()
    | `Ok msg -> return (body state msg) >?= msg_loop in
  let () = msg_loop initial_state >>= (fun () -> return (Pipe.close mailslot))
           |> don't_wait_for in
  mailslot

let post : 'a t -> 'a -> unit Deferred.t = Pipe.write

let close : 'a t -> unit = Pipe.close

(* TODO: Add timeout option to post_and_reply. Also, maybe abstract out some
 * of the common stuff to repeat less in the variations. *)
let post_and_reply t closure =
  let (reply, channel) = Pipe.create () in
  let msg = closure channel in
  post t msg >>= fun () ->
  Pipe.read reply >>= function
  | `Eof -> Deferred.Result.fail "Reply channel closed(`Eof)"
  | `Ok r -> Deferred.ok r

let post_and_reply_exn t closure =
  let (reply, channel) = Pipe.create () in
  let msg = closure channel in
  post t msg >>= fun () ->
  Pipe.read reply >>= function
  | `Eof -> failwith "Reply channel closed(`Eof)"
  | `Ok r -> return r

let post_and_reply_sync_exn t closure =
  let (reply, channel) = Pipe.create () in
  let msg = closure channel in
  post t msg >>= fun () ->
  (fun () -> Pipe.read reply) |> Thread_safe.block_on_async_exn |> function
  | `Eof -> failwith "Reply channel closed(`Eof)"
  | `Ok r -> return r

module Test = struct
  type msg = Add of int | Show | Stop | Fetch of int t

  let body state = function
    | Add i -> State (state + i)
    | Show  -> printf "state = %d\n" state; State state
    | Stop  -> Stop
    | Fetch chan -> Pipe.write chan state |> don't_wait_for; State state

  let run () =
    let mailbox = create body 0 in
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
    return (printf "Fetched state = %i" reply) >>= fun () ->
    post mailbox Stop |> don't_wait_for;
    never_returns (Scheduler.go ())
end
