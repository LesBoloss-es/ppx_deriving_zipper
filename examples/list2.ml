(** {1 Another possible implementation of list zippers} *)

open ZipperCommon


(** The type of lists can be seen as the fixpoint of the polynomial type
    [z + z * 'a * 'l] wrt. the variable ['l] where the [z]s materialise the data
    constructors and ['a] and ['l] are regular type variables. *)
type 'a t = 'a Stdlib.List.t =
  | []
  | (::) of 'a * 'a t


let pp pp_elt fmt =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
      pp_elt)


(** {2 List zippers} *)

module Zipper = struct
  type 'a list = 'a t
  (** An alias to the type of list, because we will shadow it in this module. *)

  (** {3 Definition of the zipper(s)} *)

  (** The ancestors type is common to all list zippers. It describes how to
      reconstruct the list as you move the pointer up.
      It is obtained as a list of derivatives of the polynomial defining the
      list type wrt. the fix point variable. *)
  type 'a ancestors =
    | NoAncestor
    | Cons1 of 'a * hole * 'a ancestors

  (** A [z]-zipper, or zipper wrt. constructors, is given by the sub-structure
      that is currently being pointed at (a list), and a list of ancestors. *)
  type 'a t = 'a list * 'a ancestors

  (** We could define an ['a]-zipper, or zipper wrt. the ['a], as:
      - the pointed ['a]
      - the constructor holding it with a [Hole] in place of the missing ['a]
      - the list of ancestors of this constructor.

      We never really use this type so we don't define it. However the same type
      without the value of the pointed ['a] is useful for the composability with
      other zipper types, it is denote ['a d0] below. *)

  (** This type ['a poly_da] represents list with a missing ['a] in the head
      constructor. *)
  type 'a poly_da =
    | Cons0 of hole * 'a list

  (** This type represents lists with a missing ['a] anywhere in the structure.
      Like in a zipper, the hole is accessible in constant time. *)
  type 'a d0 = 'a poly_da * 'a ancestors

  (** {3 Manipulating zippers} *)

  (** {4 Creation} *)

  (** [zip xs] creates a zipper with the pointer set to the head constructor *)
  let zip (xs: 'a list) : 'a t = (xs, NoAncestor)

  (** Filling the hole in a [d0] *)
  let fill_d0 (x: 'a) (d0: 'a d0) : 'a t =
    let Cons0 (Hole, xs), ancestors = d0 in
    (x :: xs, ancestors)


  (** {4 Moving up} *)

  (** Moving up in a zipper is defined as moving the pointer to the data
      constructor that is "one step above" the current data constructor.
      The current data constructor is always well defined:
      - in a [z]-zipper, this is the data constructor that is being pointed at;
      - in a ['a]-zipper, this is the data constructor holding the ['a] that is
        pointed at. *)

  type nonrec 'a go_up_result = ('a list, 'a t) go_up_result

  (** Go one constructor up in a [z]-zipper. *)
  let go_up (zipper : 'a t) : 'a go_up_result =
    let list, ancestors = zipper in
    match ancestors with
    | NoAncestor -> Top list
    | Cons1 (x, Hole, ancestors) -> Up (x :: list, ancestors)

  (** Same as [go_up] but raises [Invalid_argument] when there is no ancestor *)
  let go_up_exn (zipper: 'a t) : 'a t =
    match go_up zipper with
    | Top _ -> invalid_arg "Zipper.go_up_exn"
    | Up zipper -> zipper

  (** Forget the pointer and get back the unpointed structure *)
  let rec unzip (zipper: 'a t) : 'a list =
    match go_up zipper with
    | Top list -> list
    | Up zipper -> unzip zipper

  (** Go one constructor up in an ['a]-zipper.
      This is not the case for lists but, in the general case, the constructor
      above might hold several ['a]s or no ['a] at all. For this reason this
      function should not return an ['a]-zipper but a [z]-zipper. *)
  let go_up_d0 (x: 'a) (zipper: 'a d0) : 'a go_up_result =
    let Cons0 (Hole, xs), ancestors = zipper in
    go_up (x :: xs, ancestors)

  (** {4 Moving down with views} *)

  type ('a, 'view_a) generic_view =
    | []
    | (::) of 'view_a * (unit -> 'a t)

  type 'a view = ('a, 'a) generic_view

  let generic_view (view_a : 'a -> 'view_a) (zipper : 'a t) : ('a, 'view_a) generic_view =
    let list, ancestors = zipper in
    match list with
    | [] -> []
    | x :: xs -> (view_a x) :: (fun () -> xs, Cons1 (x, Hole, ancestors))

  let view zipper = generic_view Fun.id zipper
end


(** {2 Some tests} *)

let (@@) x ancestors = Zipper.Cons1 (x, Hole, ancestors)

(** [z]-zipper of the list [[1; 2; 3; 4; 5; 6]] where the constructor holding
    [4] is pointed at. *)
let example_z_zipper: int Zipper.t = ([4; 5; 6], 3 @@ 2 @@ 1 @@ NoAncestor)

(** ['a]-zipper of the list [[1; 2; 3; 4; 5; 6]] where the value [4] is pointed
    at.
    Note that the type [_ Zipper.d0] represents a punched out list but does not
    carry the information of which value has been removed/distinguished. So we
    see an ['a] zipper as a pair of an ['a] with an ['a Zipper.d0]. *)
let example_a_zipper: int * int Zipper.d0 = (
  4,
  (Cons0 (Hole, [5; 6]), 3 @@ 2 @@ 1 @@ NoAncestor)
)


let () =
  let open Zipper in

  assert (go_up example_z_zipper = go_up_d0 (fst example_a_zipper) (snd example_a_zipper));

  assert (go_up example_z_zipper = Up ([3; 4; 5; 6], 2 @@ 1 @@ NoAncestor));

  assert (go_up (go_up_exn example_z_zipper) =
    Up ([2; 3; 4; 5; 6], 1 @@ NoAncestor));

  assert (go_up (go_up_exn (go_up_exn example_z_zipper)) =
    Up ([1; 2; 3; 4; 5; 6],NoAncestor));

  assert (go_up (go_up_exn (go_up_exn (go_up_exn example_z_zipper))) =
    Top [1; 2; 3; 4; 5; 6]);

  let go_down_exn zipper =
    match view zipper with
    | [] -> invalid_arg "go_down_exn"
    | _ :: next -> next ()
  in

  assert (go_down_exn (go_up_exn example_z_zipper) = example_z_zipper);

  ()

(* An implementation of the insertion of an element in a sorted list using
   zippers. *)
let insert y list =
  let rec insert_in_zipper ((here, ancestors) as zipper) =
    match Zipper.view zipper with
    | [] -> [y], ancestors (* add y at the end and keep the same ancestors *)
    | x :: next ->
      if x < y then
        (* insert lower down *)
        insert_in_zipper (next ())
      else if y < x then
        (* insert here *)
        y :: here, ancestors
      else
        (* x = y, leave unchanged *)
        zipper
  in
  Zipper.zip list |> insert_in_zipper |> Zipper.unzip

let () =
  assert (insert 4 [1; 3; 6; 9] = [1; 3; 4; 6; 9])
