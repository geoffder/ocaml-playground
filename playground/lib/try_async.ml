open Core
open Async

module Misc = struct
  let delay_print str =
    after (Time.Span.of_sec 3.0) >>= fun () ->
    return (print_endline str)

  let delay_print_n n =
    List.init ~f:(fun i -> sprintf "i = %i" i) n
    |> List.fold ~init:(return ()) ~f:(fun f s -> f >>= fun () -> delay_print s)
end

module PrinterPipe = struct
  let rec read_loop reader () =
    Pipe.read reader >>= function
    | `Eof -> shutdown 0 |> return
    | `Ok i ->
      after (Time.Span.of_sec 1.0) >>= fun () ->
      return (printf "i = %d\n" i) >>= read_loop reader

  let fill_pipe writer ints =
    let rec loop = function
      | h :: t ->
        return (printf "writing %i\n" h) >>= fun () ->
        Pipe.write writer h >>= fun () ->
        loop t
      | [] -> return () in
    loop ints >>> fun () -> Pipe.close writer

  let run () =
    let (r, w) = Pipe.create () in
    Pipe.set_size_budget r 100;
    let _ = fill_pipe w (List.range 0 5) in
    read_loop r () |> don't_wait_for;
    never_returns (Scheduler.go ())
end

module StatePipe = struct
  type foo = { a : int list }

  let delayed_prepend_a i foo =
    after (Time.Span.of_sec 1.0) >>= fun () ->
    Deferred.return { a = i :: foo.a }

  let rec state_loop reader state =
    Pipe.read reader >>= function
    | `Eof -> List.to_string ~f:Int.to_string state.a
              |> print_endline; shutdown 0 |> return
    | `Ok f -> f state >>= state_loop reader

  let fill_pipe writer ints =
    let rec loop = function
      | h :: t -> delayed_prepend_a h |> Pipe.write writer >>= fun () -> loop t
      | [] -> return () in
    loop ints >>> fun () -> Pipe.close writer

  let run () =
    let (r, w) = Pipe.create () in
    Pipe.set_size_budget r 100;
    fill_pipe w (List.range 0 10);
    state_loop r { a = [] } |> don't_wait_for;
    never_returns (Scheduler.go ())
end
