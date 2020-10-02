open Core
open Async

(** Alias to [Async.Pipe.Writer], which serves as the input to agent. *)
type 'a t      = 'a Pipe.Writer.t

(** Alias to [Async.Pipe.Reader], used within the msg_loop of an agent to
 * receive messages to be handled by its [body]. *)
type 'a inbox  = 'a Pipe.Reader.t

(** Returned by the [body] function of an agent, providing the new state. The
 *  state can either be already resolved in [State] or wrapped in
 *  [Async.Deferred.t] in [Async]. [Stop] signals the agent to return from its
 *  msg_loop. *)
type 'b answer = State of 'b | Async of 'b Deferred.t | Stop

(** Required signature of the message handling function given to an agent on
 *  creation. Takes current agent state, and message, returning an [answer]
 *  holding either the new state, or a [Stop] signal. *)
type ('b, 'a) body = 'b -> 'a -> 'b answer

(** Alias to Async.Pipe.read, but without optional [Async.Pipe.Consumer.t]. *)
val read        : 'a inbox -> [ `Eof | `Ok of 'a ] Deferred.t

(** Alias to Async.Pipe.write. *)
val post        : 'a t -> 'a -> unit Deferred.t

(** Alias to Async.Pipe.close. *)
val close       : 'a t -> unit

(** Alias to Async.Pipe.close_read. *)
val close_inbox : 'a inbox -> unit

(** Returns the input pipe to stateful agent with given [init]ial state, and
 *  message handling [body], which takes a message ('a) and current agent state
 *  ('b) as input, and returns the new state inside an ['b answer] or a signal to
 *  [Stop] the agent. Capacity of the agent [Async.Pipe.t] can be specified with
 *  [size].*)
val create : ?size:int -> init:'b -> ('b, 'a) body -> 'a t

(** [post] a message to given agent, and do not wait for the result. Intended
 *  for use in [body] matches handling post_and_reply messages, where the channel
 *  [Async.Pipe.Writer.t] is only written to once and subsequently closed. *)
val reply  : 'a t -> 'a -> unit

(** [post_and_reply ?timeout t closure] [post]s a message created by applying
 *  the given [closure] to a [Async.Pipe.Writer.t] (for one-time use) to the
 *  given agent [t]. A reply is read from the [inbox] side of the pipe, and
 *  returned as a [Async.Deferred.Result.t]. A [timeout] in seconds can be
 *  specified, which will cause an early return with error ["Timeout."]. *)
val post_and_reply
  :  ?timeout:float
  -> 'a t
  -> ('b t -> 'a)
  -> ('b, string) result Deferred.t

(** [post_and_reply], but throws an exception.*)
val post_and_reply_exn      : 'a t -> ('b t -> 'a) -> 'b Deferred.t

(** [post_and_reply], but blocks on the agent's reply with
 *  Thread_safe.block_on_async, so that one can use it in otherwise synchronous
 *  code. NOTE: This is probably a bad idea. *)
val post_and_reply_sync     : 'a t -> ('b t -> 'a) -> ('b, string) result

(** [post_and_reply_sync], but throws an exception. *)
val post_and_reply_sync_exn : 'a t -> ('b t -> 'a) -> 'b
