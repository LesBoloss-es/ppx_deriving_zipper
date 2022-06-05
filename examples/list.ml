(** {1 A possible implementation of list zippers} *)


(** The type of lists can be seen as the fixpoint of the polynomial type
    [z + z * 'a * 'l] wrt. the variable ['l] where the [z]s materialise the data
    constructors and ['a] and ['l] are regular type variables. *)
type 'a t = 'a Stdlib.List.t =
  | []
  | (::) of 'a * 'a t


(** {2 List zippers} *)

module Zipper = struct

  (** {3 General zipper utilities}

      NB: This section is not specific to lists *)

  type hole = Hole
  (** We see a zipper, aka a data structure with a distinguished element, as a
      pair of a structure with a hole in place of that distinguished element,
      and that element on the side.
      There is no need to physically represent this holes in the type of zippers
      but this helps readability.
      This is the type of holes. *)

  type z = Z
  (** The type [z] tags derivatives wrt. data constructors. *)

  type 'a var = Var of 'a [@@unboxed]
  (** The type ['a var] tags derivatives wrt. ['a] *)

  type self = |
  (** The type [self] tags derivatives wrt. the type itself, or better said, wrt.
      the variable we use to compute the type as a fix point of a polynomial. *)
  (* FIXME: is it a good name? *)
  (* FIXME: this probably won't scale up well to mutually recursive types. *)


  (** {3 Definition of the zipper(s)} *)

  (** The type [('x, 'a) head] represents lists where a hole has been
      "punched" somewhere in the head constructor.

      Algebraically, this corresponds to differentiating the polynomial
      [z + z * 'a * 'l] (defining the type list by fix point) by some variable
      and then applying it to ['l = 'a List.t].

      The type ['x] represents the variable with respect to which the derivative
      has been taken. This is "the type of the hole".
      It can either be:
        - [z] for derivatives wrt. data constructors;
        - ['a var] for derivatives wrt. ['a];
        - [self] for derivatives wrt. ['l]. *)
  type ('missing, 'a) head =
    | Raw: 'a t -> (z, 'a) head
    (** A constructor ([Z] of type [z]) has been removed.
        NB: Alternatively we could have listed the two list constructors of but
        this yields a more concise [head] type. *)
    | Cons0: hole * 'a t -> ('a var, 'a) head
    (** An ['a] has been removed. *)
    | Cons1: 'a * hole -> (self, 'a) head
    (** An ['a list] has been removed. *)

  (** An ancestor is a constructor of the list type in which an occurrence
      of ['a List.t] have been removed.
      This corresponds precisely to a derivative of [z + z * 'a * 'l] wrt. ['l]
      aka a [(self, 'a) head].

      This types reprensents lists of ancestors. *)
  type 'a ancestors =
    | NoAncestor
    | Ancestor of (self, 'a) head * 'a ancestors

  (** A zipper is a list in which a particular location is distinguished (be it
      a data constructor or an ['a]).
      In all cases this can be represented as:
        - the pointed value ([pointee]);
        - the constructor holding this value ([constructor]);
        - its ancetors ([ancestors]).

      NB1: This type captures zippers wrt to data constructors (when ['x = z])
      and zippers wrt to ['a]s (when ['x = 'a var]).
      This allows to define zipper operations in a generic manner.

      NB2: Since ['x] can only be inhabited by [z] and ['a var] (not [self]) in
      this definition, only derivatives wrt. [z] or ['a] can occur in
      [constructor]. This is exactly what we want.
      FIXME: this is a subtle but important point, is it clear enough?

      NB3: Alternatively we could have excluded the [Cons1] constructor from the
      [head] type. *)
  type ('x, 'a) t = {
    pointee: 'x;
    constructor: ('x, 'a) head;
    ancestors: 'a ancestors;
  }


  (** {3 Manipulating zippers} *)

  (** {4 Creation} *)

  (** [zip xs] creates a zipper with the pointer set to the head constructor *)
  let zip (xs: 'a list) : (z, 'a) t = {
    pointee = Z;
    constructor = Raw xs;
    ancestors = NoAncestor
  }


  (** {4 Moving up} *)

  (** Moving up in a zipper is defined as moving the pointer to the data
      constructor that is "one step above" the current data constructor.
      The current data constructor is always well defined:
      - in a [z]-zipper, this is the data constructor that is being pointed at;
      - in a ['a]-zipper, this is the data constructor holding the ['a] that is
        pointed at. *)

  (** Moving up in a zipper may have two possible outcomes:
      - either we already are at the root of the structure, in this case we
        forget the pointer and return the un-pointed structure;
      - or we can go up one constructor and return the new zipper.

    Note that in both cases we always return a [z]-zipper.
    This is because, in the general case (not the case for lists), the
    constructor above the current constructor may not hold any ['a], or may hold
    several of them. The only natural operation when moving up is thus to point
    at the new constructor. *)
  type 'a go_up_result =
    | Top of 'a list
    | Up of (z, 'a) t
      (* FIXME: It is tempting to inline the definition of [(z, 'a) list_zipper]
         here and to remove the useless [z] to get this instead:
         [Up of 'a list * 'a list_ancestors].
         What should we do?
         - On one hand we have a ligher type and less boxing.
         - On the other hand chaining [go_up]s is cumbersome *)

  (** Go up one constructor in a zipper of any kind. *)
  let go_up: type x a. (x, a) t -> a go_up_result = fun zipper ->
    (* If the pointer is on an alpha, first move it to the constructor (re-plug
       the missing part inside the [Cons]. *)
    let head: a list =
      match zipper.constructor, zipper.pointee with
      | Raw head, Z -> head
      | Cons0 (Hole, list), Var x -> x :: list
      | Cons1 _, _ -> .
    in
    (* Go up one constructor. *)
    match zipper.ancestors with
    | NoAncestor -> Top head
    | Ancestor (Cons1 (x, Hole), ancestors) ->
      Up {pointee = Z; constructor = Raw (x :: head); ancestors}

  (** Same as [go_up] but raises [Invalid_argument] when there is no ancestor *)
  let go_up_exn: type x a. (x, a) t -> (z, a) t = fun zipper ->
    match go_up zipper with
    | Top _ -> invalid_arg "go_up_exn"
    | Up zipper -> zipper

  (** Forget the pointing and get back the unpointed structure *)
  let rec unzip: type x a. (x, a) t -> a list = fun zipper ->
    match go_up zipper with
    | Top list -> list
    | Up zipper -> unzip zipper


  (** {4 Moving down with views} *)

  type 'a view =
    | []
    | (::) of 'a * (unit -> (z, 'a) t)

  let view: type x a. (x, a) t -> a view = fun zipper ->
    match zipper.constructor, zipper.pointee with
    | Raw [], Z -> []
    | Raw (x :: xs), Z -> x :: (fun () -> {
        pointee = Z;
        constructor = Raw xs;
        ancestors = Ancestor (Cons1 (x, Hole), zipper.ancestors);
      })
    | Cons0 (Hole, xs), Var x -> x :: (fun () -> {
        pointee = Z;
        constructor = Raw xs;
        ancestors = Ancestor (Cons1 (x, Hole), zipper.ancestors);
      })
    | Cons1 _, _ -> .
end


(* NB. I moved the printers at the end of the file to de-clutter the code *)
module Print = struct
  (** {2 Printers} *)

  let hole fmt Zipper.Hole = Format.fprintf fmt "◼"

  let z fmt Zipper.Z = Format.fprintf fmt "z"

  let list pp_elt fmt =
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
         pp_elt )

  let pp_head: type x a.
    (Format.formatter -> a -> unit) ->
    Format.formatter ->
    (x, a) Zipper.head -> unit
  = fun pp_elt fmt head ->
    match head with
    | Raw l ->
      Format.fprintf fmt "Raw %a" (list pp_elt) l
    | Cons0 (Hole, l) ->
      Format.fprintf fmt "Cons0 (%a, %a)" hole Hole (list pp_elt) l
    | Cons1 (x, Hole) ->
      Format.fprintf fmt "Cons1 (%a, %a)" pp_elt x hole Hole

  let list_ancestors pp_elt fmt ancestors =
    let rec to_list: _ Zipper.ancestors -> _ list = function
      | NoAncestor -> []
      | Ancestor (cons, ancestors) -> cons :: to_list ancestors
    in
    list (pp_head pp_elt) fmt (to_list ancestors)
end

(** {2 Some tests} *)

let (@@) anc ancs = Zipper.Ancestor (anc, ancs)
(* There are not many right-associative operators in OCaml... *)

(** [z]-zipper of the list [[1; 2; 3; 4; 5; 6]] where the constructor holding
    [4] is pointed at. *)
let example_z_zipper: (Zipper.z, int) Zipper.t = {
  pointee = Z;
  constructor = Raw [4; 5; 6];
  ancestors = Cons1 (3, Hole) @@ Cons1 (2, Hole) @@ Cons1 (1, Hole) @@ NoAncestor
}

(** ['a]-zipper of the list [[1; 2; 3; 4; 5; 6]] where the integer [4] is
    pointed at. *)
let example_a_zipper: (int Zipper.var, int) Zipper.t = {
  pointee = Var 4;
  constructor = Cons0 (Hole, [5; 6]);
  ancestors = Cons1 (3, Hole) @@ Cons1 (2, Hole) @@ Cons1 (1, Hole) @@ NoAncestor
}


let () =
  let open Zipper in

  assert (go_up example_z_zipper = go_up example_a_zipper);

  assert (go_up example_z_zipper = Up {
    pointee = Z;
    constructor = Raw [3; 4; 5; 6];
    ancestors = Cons1 (2, Hole) @@ Cons1 (1, Hole) @@ NoAncestor
  });

  assert (go_up (go_up_exn example_z_zipper) = Up {
    pointee = Z;
    constructor = Raw [2; 3; 4; 5; 6];
    ancestors = Cons1 (1, Hole) @@ NoAncestor;
  });

  assert (go_up (go_up_exn (go_up_exn example_z_zipper)) = Up {
    pointee = Z;
    constructor = Raw [1; 2; 3; 4; 5; 6];
    ancestors = NoAncestor;
  });

  assert (go_up (go_up_exn (go_up_exn (go_up_exn example_z_zipper))) = Top
    [1; 2; 3; 4; 5; 6]
  );

  let go_down_exn zipper =
    match view zipper with
    | [] -> invalid_arg "go_down_exn"
    | _ :: next -> next ()
  in

  assert (go_down_exn (go_up_exn example_z_zipper) = example_z_zipper);

  ()

(* An implementation of the insertion of an element in a sorted list using
   zippers.
   Pros: it's tailrec
   Cons: it's weird

   FIXME: I don't like that I have to know the definition of the zipper type to
   be able to write this function. *)
let insert y list =
  let rec insert_in_zipper zipper =
    match Zipper.view zipper with
    | [] ->
      {zipper with constructor = (Zipper.zip [y]).constructor}
    | x :: next ->
      if x < y then
        insert_in_zipper (next ())
      else if y < x then
        let Raw here = zipper.constructor in
        {zipper with constructor = Raw (y :: here)}
      else
        zipper
  in
  Zipper.zip list |> insert_in_zipper |> Zipper.unzip

let () =
  assert (insert 4 [1; 3; 6; 9] = [1; 3; 4; 6; 9])
