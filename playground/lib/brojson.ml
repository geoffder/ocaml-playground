open Base

(* Playing around with GADTs (generalized algebraic data types). Seeing what some
 * of the tricks that you could do in a Yojson style JSON type if using GADTs
 * rather than regular algebraic types. *)

let ( >> ) f g x = g (f x)

(* GADT definition for our JSON type. Using a subset of the constructors
 * available in Yojson. *)
type _ t =
  | S : string -> string t
  | I : int -> int t
  | F : float -> float t
  | L : 'a t list -> 'a t list t
  | A : (string * 'a t) list -> (string * 'a t) list t

(* Auto-matically unpack one level of the JSON. Note that the return type can be
 * untagged and polymorphic. Bare string, int, float, can be returned, along with
 * lists or alists containing elements of type t. *)
let auto : type a. a t -> a = function
  | S s -> s
  | I i -> i
  | F f -> f
  | L l -> l
  | A a -> a

(* Due to the power of GADTs, the single entry pattern match in the argument
 * is actually exhaustive. This function can only be applied to L lists, and each
 * of it's elements will also be automatically unwrapped (one level). *)
let auto_list : type a. a t list t -> a list = fun (L l) -> List.map ~f:auto l

let auto_alist : type a. (string * a t) list t -> (string * a) list =
  fun (A al) -> List.map ~f:(fun (k, v) -> (k, auto v)) al

let member key =
  auto_alist
  >> List.find ~f:(fun (k, _) -> String.equal key k)
  >> Option.map ~f:snd

(* Unfortunately it seems the constraints that I put on A and L to get them
 * working before have restricted me from having mixed associative list types,
 * so I can't use them as needed for JSON like modelling. Need to play with again
 * sometime to see if I can figure out a way around that.
 *
 * The first hurlde is my auto function, which wants to be able to unwrap all
 *  the types in t. This doesn't work with mixed value types in A (or L, though
 * that one isn't desirable to have mixed anyway), since unwrapping from A will
 * put unwrapped mixed tuple types into a lisit (which is obviously illegal).
 * Thus, why making the type parameter a hole (_) rather than 'a caused the
 * compiler to complain about the types of L and A in the auto function. *)

let j_al = A [ ("bar", L [ S "test"; S "foo"]); ("baz", L []) ]

let l = member "bar" j_al |> Option.map ~f:(List.map ~f:auto)
