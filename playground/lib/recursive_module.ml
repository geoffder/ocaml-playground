(* By explicitly defining a signature and using the rec keyword, we can define
 * modules where children can reference the parent type (which is itself
 * dependent on that, and the other children). It's wordy, but doable.
 *
 * Also, note that this must be done in the ml source file itself, the compiler
 * will not use the mli signature to resolve the recursiveness. If using an mli,
 * you'll want to copy the sinature over there as well. *)

module rec Parent : sig
  module Foo : sig
    type t = Foo | Bar
  end

  module Bar : sig
    type t = A | B
  end

  module Child : sig
    type t = Parent of Parent.t
  end

  type t = F of Foo.t | B of Bar.t | C of Child.t
end = struct
  module Foo = struct
    type t = Foo | Bar
  end

  module Bar = struct
    type t = A | B
  end

  module Child = struct
    type t = Parent of Parent.t
  end

  type t = F of Foo.t | B of Bar.t | C of Child.t
end

let c = Parent.C (Parent (F Foo))
