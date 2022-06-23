(** {1 A possible implementation of tree zippers}

    This demonstrates the composability with other zippers (here: that of lists)
*)

open ZipperCommon


(** An artificial example of trees with two constructors: [Leaf] of arity zero
    and [Node] carrying a nonempty list of trees encoded as a pair of a tree and
    a list of trees.

    This type is the fix point of the type [z + z * 't * 't list] with respect
    to ['t]. *)
type t =
  | Leaf
  | Node of t * t List2.t
[@@deriving show {with_path=false}]


(** {2 Tree zippers} *)

module Zipper = struct
  type tree = t
  (** An alias to the type of trees, because we will shadow it in this module. *)

  (** {3 Zipper for Trees} *)

  (** The ancestors type describes how to reconstruct the list as you move the
      pointer up.
      It is obtained as a list of derivatives of the type defining the type of
      trees by fix point wrt. the fix point variable. *)
  type ancestors =
    | NoAncestor
    | Node0 of hole * tree List2.t * ancestors
    | Node1 of tree * tree List2.Zipper.d0 * ancestors

  (** In a tree zipper, the pointer might either be on a tree constructor
      ([Leaf] or [Node]) or on a list constructor on the right hande side of a
      [Node].
      This types lists all the possibilities. *)
  type head =
    | Raw of tree (* FIXME: I don't like the name [Raw] *)
    | InNode1 of tree * tree List2.Zipper.t

  (** A tree zipper is thus given by a [head] carrying the pointer and a list of
      ancestors. *)
  type t = head * ancestors

  (** {3 Manipulating zippers} *)

  (** {4 Creation} *)

  (** [zip xs] creates a zipper with the pointer set to the head constructor *)
  let zip (tree: tree) : t = (Raw tree, NoAncestor)


  (** {4 Moving up} *)

  (** Moving up in a zipper is defined as moving the pointer to the data
      constructor that is "one step above" the currently pointed data
      constructor.
      - If the current pointer is on a tree constructor, we go up one ancestor.
      - If the current pointer is somewhere in the list of children of a [Node],
        move it up one list constructor. *)

  type nonrec go_up_result = (tree, t) go_up_result

  (** Go one constructor up in a [z]-zipper. *)
  let go_up: t -> go_up_result = function
    (* The pointer is on a tree constructor, go up one ancestor. *)
    | Raw tree, NoAncestor ->
      (* There is no ancestor: we are at the top of the structure. *)
      Top tree
    | Raw tree, Node0 (Hole, trees, ancestors) ->
      (* the next ancestor has a hole in [Node (Hole, ...)]: plug the current
         tree in this hole and set the pointer to the new [Node]. *)
      Up (Raw (Node (tree, trees)), ancestors)
    | Raw tree, Node1 (tree', list_d0, ancestors) ->
      (* the next ancestor has a hole somewhere in the right list: plug the
         current tree in this hole and set the pointer to the list constructor
         carrying this [Hole]. *)
      let list_zipper = List2.Zipper.fill_d0 tree list_d0 in
      Up (InNode1 (tree', list_zipper), ancestors)

    (* The pointer is in the list in the rhs of [Node]. *)
    | InNode1 (tree, list_zipper), ancestors ->
      (* Go up one step in the list. If reach the top of the list, move the
         pointer to the [Node] constructor. *)
      match List2.Zipper.go_up list_zipper with
      | Top trees -> Up (Raw (Node (tree, trees)), ancestors)
      | Up list_zipper -> Up (InNode1 (tree, list_zipper), ancestors)
end
