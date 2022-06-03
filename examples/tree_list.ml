(** {1 Example} *)

type hole = Hole
[@@deriving show {with_path=false}]

(** Result of a [go_up] operation: either already at the [Top], returning the
    unzipped zipper, or went [Up] once. *)
type ('typ, 'zipper_type) go_up_result =
  | Top of 'typ
  | Up of 'zipper_type
[@@deriving show {with_path=false}]

(* {2 Lists} *)

type 'a mlist =
  | Nil
  | Cons of 'a * 'a mlist
[@@deriving show {with_path=false}]

let (//) x l = Cons (x, l)

module ListZipper = struct
  (** {3 Zipper of Lists} *)

  type 'a ancestors =
    | NoAncestor
    | ACons of 'a * hole * 'a ancestors
  [@@deriving show {with_path=false}]

  (** {4 Derivative wrt. ['a]} *)

  type 'a poly_d0 = D0Cons of hole * 'a mlist
  [@@deriving show {with_path=false}]
  type 'a d0 = 'a poly_d0 * 'a ancestors
  [@@deriving show {with_path=false}]

  (** {4 Zipper}

      (more or less the same as the derivative wrt. [z]) *)

  (** NOTE: this would actually require a [raw]. *)
  type 'a head = 'a mlist
  [@@deriving show {with_path=false}]

  type 'a t = 'a head * 'a ancestors
  [@@deriving show {with_path=false}]

  (** {4 [go_up]s} *)

  let go_up_d0 (x : 'a) ((head, ancestors) : 'a d0)
    : ('a mlist, ('a * 'a d0)) go_up_result
    =
    let D0Cons (Hole, list) = head in
    match ancestors with
    | NoAncestor -> Top (Cons (x, list))
    | ACons (x', Hole, ancestors) ->
      Up (x', (D0Cons (Hole, Cons (x, list)), ancestors))

  let go_up ((head, ancestors) : 'a t) : ('a mlist, 'a t) go_up_result =
    (* NOTE: we'd have to match on [Raw]/non-[Raw] but everything is [Raw]. *)
    match ancestors with
    | NoAncestor ->
      Top head
    | ACons (x, Hole, ancestors) ->
      Up (Cons (x, head), ancestors)
end

(** {2 Trees} *)

type tree =
  | Leaf
  | Node of tree * tree mlist
[@@deriving show {with_path=false}]

module TreeZipper = struct
  (** {3 Zipper for Trees} *)

  type ancestors =
    | NoAncestor
    | ANodeL of hole * tree mlist * ancestors
    | ANodeR of tree * tree ListZipper.d0 * ancestors
  [@@deriving show {with_path=false}]

  (** {4 Zipper} *)

  type head =
    | Raw of tree
    | PtdNode of tree * tree ListZipper.t
  [@@deriving show {with_path=false}]

  type t = head * ancestors
  [@@deriving show {with_path=false}]

  let go_up ((head, ancestors) : t) : (tree, t) go_up_result =
    match head with
    | Raw head ->
      (
        match ancestors with
        | NoAncestor ->
          Top head
        | ANodeL (Hole, trees, ancestors) ->
          Up (Raw (Node (head, trees)), ancestors)
        | ANodeR (tree, list_d0, ancestors) ->
          match ListZipper.go_up_d0 head list_d0 with
          | Top trees ->
            Up (Raw (Node (tree, trees)), ancestors)
          | Up (head, list_d0) ->
            Up (Raw head, ANodeR (tree, list_d0, ancestors))
      )
    | PtdNode (tree, list_zipper) ->
      (
        match ListZipper.go_up list_zipper with
        | Top trees ->
          Up (Raw (Node (tree, trees)), ancestors)
        | Up list_zipper ->
          Up (PtdNode (tree, list_zipper), ancestors)
      )
end
