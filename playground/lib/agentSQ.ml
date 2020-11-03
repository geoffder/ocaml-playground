open Core
open Async

type 'a t      = 'a Squeue.t
type 'a inbox  = 'a Pipe.Reader.t

type 'b answer           = State of 'b | Stop
type ('b, 'a) msg_handler = 'b -> 'a -> 'b answer
type ('a, 'b) body        = 'a t -> ('b, 'a) msg_handler

let read inbox  = Squeue.pop inbox
let post        = Squeue.push

let ( >> ) f g x = g (f x)

let ( >?= ) answer loop =
  match answer with
  | State s -> loop s
  | Stop    -> ()

let create ?(size=100) ~init body =
  let inbox   = Squeue.create size in
  let handler = body inbox in
  let rec msg_loop state = read inbox |> handler state >?= msg_loop in
  In_thread.run (fun () -> msg_loop init) |> don't_wait_for;
  inbox

let reply channel = post channel

let result_of_timeout = function
  | `Result a -> Result.return a
  | `Timeout  -> Result.fail "Timeout."

let post_and_reply ?(timeout=86400.) t closure =
  let timer m = with_timeout (Time.Span.of_sec timeout) m >>| result_of_timeout in
  let channel = Squeue.create 1 in
  let inbox = In_thread.pipe_of_squeue channel in
  let msg = closure channel in
  post t msg;
  Pipe.read inbox |> timer >>| (fun r -> Pipe.close_read inbox; r) >>=? function
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
