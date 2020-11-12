open Core

module Incr : Incremental.S = Incremental.Make ()

module MathStack = struct
  (* Toy example with a "stack" list l that one can push functions that do
   * artihmetic on integers that will be used to generate a new observed value a.
   * Mulitple functions pushed to the stack will be composed on stabilization, and
   * the number stored in v will be passed in.
   *
   * This is a bit janky though, since `react` actually changes the Var.t, thus
   * an immediate stabilize will trigger another observation.
   *
   * NOTE:
   * I've been reminded by playing around here that since Incremental is built for
   * DAGS, a node (like a) cannot be used in the calculation of itself
   * (cyclic dependency). *)

  let push_fun stack f = Incr.Var.replace ~f:(fun s -> f :: s) stack

  let l = Incr.Var.create []
  let v = ref 5

  let react fun_stack =
    Incr.Var.set l [];
    let math = List.fold ~init:(( * ) 1) ~f:Fn.compose fun_stack in
    math !v

  let a = Incr.map (Incr.Var.watch l) ~f:react
  let a_o = Incr.observe a

  let print_a () =
    Incr.Observer.value a_o
    |> Result.ok
    |> Option.value_map ~f:Int.to_string ~default:"Failed observation of a."
    |> Stdio.print_endline

  let run () =
    push_fun l (( * ) 10);
    Incr.stabilize ();
    print_a ();
    push_fun l (( * ) 5);
    Incr.stabilize ();
    print_a ();
    push_fun l (( + ) 5);
    push_fun l (( * ) 10);
    push_fun l (( * ) 5);
    Incr.stabilize ();
    print_a ()

  let update_to_string to_string u =
    let open Incr.Observer.Update in
    match u with
    | Initialized v    -> "initialized to " ^  to_string v
    | Changed (v1, v2) -> "changed from " ^ to_string v1 ^ " to " ^ to_string v2
    | Invalidated      -> "invalidated!"

  let print_update to_string u = print_endline (update_to_string to_string u)

  let run_updates () =
    Incr.Observer.on_update_exn ~f:(print_update Int.to_string) a_o;
    Incr.stabilize ();
    push_fun l (( * ) 10);
    Incr.stabilize ();
    Incr.stabilize ();
    push_fun l (( * ) 5);
    Incr.stabilize ();
    push_fun l (( + ) 5);
    push_fun l (( * ) 10);
    push_fun l (( * ) 5);
    Incr.stabilize ()
end

module MapDiffs = struct
  module Incr_map = Incr_map.Make(Incr)

  let m_v = Incr.Var.create (Map.empty (module Int))
  let m_t = Incr.Var.watch m_v
  let m_o = Incr.observe m_t

  let map_to_string m =
    let seq = Map.to_sequence m in
    if Sequence.length seq = 0
    then "( empty )\n"
    else
      let f acc (k, v) = sprintf "%s%i => %s\n" acc k v in
      Sequence.fold ~init:"" ~f seq

  let string_repr = Incr.map m_t ~f:(fun m -> "Map:\n" ^ map_to_string m)
  let string_repr_o = Incr.observe string_repr

  let updated_repr_obs update =
    let open Incr.Observer.Update in
    match update with
    | Initialized s  -> print_endline s
    | Changed (_, s) -> print_endline s
    | Invalidated    -> print_endline "invalidated!"

  let print_contents m = print_endline (map_to_string m)

  let print_diff seq =
    let f (k, diff) =
      match diff with
      | `Left v           -> printf "removed: %i => %s\n" k v
      | `Right v          -> printf "added: %i => %s\n" k v
      | `Unequal (v1, v2) -> printf "changed: %i => (%s -> %s)\n" k v1 v2
    in
    print_endline "changes made to map...";
    Sequence.iter ~f seq

  let symm_diff = Map.symmetric_diff ~data_equal:String.equal

  let new_entries_obs update =
    let open Incr.Observer.Update in
    match update with
    | Initialized m    -> print_endline "map initialized to..."; print_contents m
    | Changed (m1, m2) -> print_diff (symm_diff m1 m2)
    | Invalidated      -> print_endline "invalidated!"

  let new_entries update =
    let open Incr.Update in
    match update with
    | Necessary m      ->
      print_endline "necessary: map initialized to..."; print_contents m
    | Unnecessary      -> print_endline "unecessary?"
    | Changed (m1, m2) -> print_diff (symm_diff m1 m2)
    | Invalidated      -> print_endline "invalidated!"

  let run () =
    Incr.stabilize ();
    Incr.Var.replace ~f:(fun m -> Map.set m ~key:0 ~data:"foo") m_v;
    Incr.stabilize ();
    Incr.Var.replace ~f:(fun m -> Map.set m ~key:1 ~data:"bar") m_v;
    Incr.Var.replace ~f:(fun m -> Map.set m ~key:2 ~data:"baz") m_v;
    Incr.stabilize ();
    Incr.Var.replace ~f:(fun m -> Map.remove m 0) m_v;
    Incr.Var.replace ~f:(fun m -> Map.set m ~key:1 ~data:"buzz") m_v;
    Incr.stabilize ();
    Incr.stabilize ()

  let run_diff () =
    Incr.on_update ~f:new_entries m_t;
    run ()

  let run_diff_obs () =
    Incr.Observer.on_update_exn ~f:new_entries_obs m_o;
    run ()

  let run_repr () =
    Incr.Observer.on_update_exn ~f:updated_repr_obs string_repr_o;
    run ()
end
