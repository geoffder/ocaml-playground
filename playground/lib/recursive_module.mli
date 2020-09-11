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
end

val c : Parent.t
