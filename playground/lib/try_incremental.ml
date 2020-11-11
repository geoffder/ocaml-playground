open Core

module Inc : Incremental.S = Incremental.Make ()

(* Toy example with a "stack" list l that one can push functions that do
 * artihmetic on integers that will be used to generate a new observed value a.
 * Mulitple functions pushed to the stack will be composed on stabilization, and
 * the number stored in v will be passed in.
 *
 * NOTE:
 * I've been reminded by playing around here that since Incremental is built for
 * DAGS, a node (like a) cannot be used in the calculation of itself
 * (cyclic dependency). Thus my thought of using incremental to update client
 * state (which depends on previous client state) is likely ill-founded. I think
 * Async.Pipe might be a place to look though? *)

let push_fun stack f = Inc.Var.set stack (f :: (Inc.Var.value stack))

let l = Inc.Var.create []
let v = ref 5

let react fun_stack =
  Inc.Var.set l [];
  let math = List.fold ~init:(( * ) 1) ~f:Fn.compose fun_stack in
  math !v

let a = Inc.map (Inc.Var.watch l) ~f:react
let a_o = Inc.observe a

let print_a () =
  Inc.Observer.value a_o
  |> Result.ok
  |> Option.value_map ~f:Int.to_string ~default:"Failed observation of a."
  |> Stdio.print_endline

let run () =
  push_fun l (( * ) 10);
  Inc.stabilize ();
  print_a ();
  push_fun l (( * ) 5);
  Inc.stabilize ();
  print_a ();
  push_fun l (( + ) 5);
  push_fun l (( * ) 10);
  push_fun l (( * ) 5);
  Inc.stabilize ();
  print_a ()

let update_to_string to_string u =
  let open Inc.Observer.Update in
  match u with
  | Initialized v    -> "initialized to " ^  to_string v
  | Changed (v1, v2) -> "changed from " ^ to_string v1 ^ " to " ^ to_string v2
  | Invalidated      -> "invalidated!"

let print_update to_string u = print_endline (update_to_string to_string u)

let run_with_updates () =
  Inc.Observer.on_update_exn ~f:(print_update Int.to_string) a_o;
  Inc.stabilize ();
  push_fun l (( * ) 10);
  Inc.stabilize ();
  push_fun l (( * ) 5);
  Inc.stabilize ();
  push_fun l (( + ) 5);
  push_fun l (( * ) 10);
  push_fun l (( * ) 5);
  Inc.stabilize ()
