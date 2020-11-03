open Core
open Async

type 'a t      = 'a Pipe.Writer.t
type 'a inbox  = 'a Pipe.Reader.t

type 'b answer           = State of 'b | Async of 'b Deferred.t | Stop
type ('b, 'a) msg_handler = 'b -> 'a -> 'b answer
type ('a, 'b) body        = 'a t -> ('b, 'a) msg_handler

let read inbox  = Pipe.read inbox
let post        = Pipe.write
let close       = Pipe.close
let close_inbox = Pipe.close_read

let ( >> ) f g x = g (f x)

let ( >?= ) answer loop =
  match answer with
  | State s -> loop s
  | Async d -> d >>= loop
  | Stop    -> return ()

let create ?(size=100) ~init body =
  let (inbox, mailslot) = Pipe.create () in
  let () = Pipe.set_size_budget inbox size in
  let handler = body mailslot in
  let rec msg_loop state =
    read inbox >>= function
    | `Eof    -> return ()
    | `Ok msg -> handler state msg >?= msg_loop in
  let () = upon (msg_loop init) (fun () -> close mailslot) in
  mailslot

let create_in_thread ?(size=100) ~init body =
  let (inbox, mailslot) = Pipe.create () in
  let () = Pipe.set_size_budget inbox size in
  let handler = body mailslot in
  let rec msg_loop state =
    read inbox >>= function
    | `Eof    -> return ()
    | `Ok msg -> handler state msg >?= msg_loop in
  (* NOTE: Tried this (against docs recommendation), seemed to still block up the
   * main thread with the heavy comp tried int the Crunch test module, so the thread
   * was indeed not actually executing in parellel. Maybe because the message loop
   * itself is in Async, using Pipe, which is obviously against the no Async
   * rule in the docs. If I try again, do it with an agent body void of Async.
   *
   * Looks like Squeue is a thread-safe pipe that blocks until there is something
   * available, thus I could build a message loop with it that does not require
   * async. Can use In_thread.pipe_of_squeue to turn an Squeue.t to a
   * Pipe.Reader.t, allowing async reply channels. Squeue.push (or never blocking
   * alternatives) can be used to send messages in. *)
  let () = upon
      (Deferred.join (In_thread.run (fun () -> msg_loop init)))
      (fun () -> close mailslot) in
  mailslot
let reply channel = post channel >> don't_wait_for

let result_of_timeout = function
  | `Result a -> Result.return a
  | `Timeout  -> Result.fail "Timeout."

let post_and_reply ?(timeout=86400.) t closure =
  let timer m = with_timeout (Time.Span.of_sec timeout) m >>| result_of_timeout in
  let (inbox, channel) = Pipe.create () in
  let msg = closure channel in
  post t msg >>= fun () ->
  read inbox |> timer >>| (fun r -> close_inbox inbox; r) >>=? function
  | `Eof  -> Deferred.Result.fail "Reply channel unexpectedly closed (`Eof)"
  | `Ok r -> Deferred.Result.return r

let post_and_reply_exn t closure =
  post_and_reply t closure >>| Result.ok_or_failwith

(* NOTE: Warning, shouldn't be using block_on_async within async code. *)
let post_and_reply_sync t closure =
  (fun () -> post_and_reply t closure)
  |> Thread_safe.block_on_async
  |> Result.map_error ~f:(Error.of_exn >> Error.to_string_hum)
  |> Result.join

(* NOTE: Warning, shouldn't be using block_on_async within async code. *)
let post_and_reply_sync_exn t closure =
  (fun () -> post_and_reply_exn t closure) |> Thread_safe.block_on_async_exn
