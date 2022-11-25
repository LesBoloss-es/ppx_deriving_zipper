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

  let go_up_exn zipper =
    match go_up zipper with
    | Up zipper -> zipper
    | Top _ -> invalid_arg "Tree_list.Zipper.go_up_exn"

  type here =
    | Here of (unit -> t)
    | NotHere of (unit -> t)

  type up_view =
    | Top of tree
    | Node of here * here
    | Cons of here * here

  let up (zipper : t) : up_view =
    (* FIXME: on peut spécialiser *)
    let here = Here (fun () -> go_up_exn zipper) in
    match zipper with
    | Raw tree, NoAncestor -> Top tree

    | Raw tree, Node0 (Hole, trees, ancestors) ->
        Node (
          here,
          NotHere (fun () -> InNode1 (tree, List2.Zipper.zip trees), ancestors)
        )

    | Raw tree, Node1 (tree', list_d0, ancestors) ->
        Cons (
          here,
          NotHere (fun () ->
            match List2.Zipper.(view (fill_d0 tree list_d0)) with
            | [] -> assert false
            | _ :: xs -> InNode1 (tree', xs ()), ancestors)
        )

    | InNode1 (tree, list_zipper), ancestors ->
      match List2.Zipper.go_up list_zipper with
      | Top trees ->
          Node (
            NotHere (fun () -> Raw tree, Node0 (Hole, trees, ancestors)),
            here
          )
      | Up list_zipper ->
          Cons (
            NotHere (fun () -> InNode1 (tree, list_zipper), ancestors),
            here
          )

  (** {4 Moving down} *)

  type down_view =
    | Leaf
    | Node of (unit -> t) * (unit -> t)
    | Nil
    | Cons of (unit -> t) * (unit -> t)

  let down ((head, ancestors) : t) : down_view =
    match head with
    | Raw Leaf -> Leaf
    | Raw (Node (t, ts)) ->
      Node ((fun () -> (Raw t, Node0 (Hole, ts, ancestors))),
            (fun () -> (InNode1 (t, List2.Zipper.zip ts), ancestors)))
    | InNode1 (t, l_zipper) ->
      let (ts, l_ancestors) = l_zipper in
      match ts with
      | [] -> Nil
      | t' :: ts' ->
        Cons (
          (fun () ->
            (Raw t', Node1 (t, (Cons0 (Hole, ts'), l_ancestors), ancestors))),
          (fun () ->
            (InNode1 (t, (ts', Cons1 (t', Hole, l_ancestors))), ancestors)))
end


(** {2 Examples} *)

(** {3 How to Rewrite Recursive Algorithms for Dummies} *)

let exists path tree =
  let rec exists (path : int list) (tree : Zipper.t) =
    match path with
    | [] -> true
    | n :: path ->
      assert (n >= 0);
      match Zipper.down tree with
      | Leaf | Nil -> false
      | (Node (tree, _) | Cons (tree, _))  when n = 0 -> exists path (tree ())
      | (Node (_, trees) | Cons (_, trees)) ->
        exists ((n - 1) :: path) (trees ())
  in

  if List.exists ((>) 0) path then
    invalid_arg "Tree_list.exists";

  exists path (Zipper.zip tree)

(** {3 Move in Four Directions}

   - We always point on a constructor of [tree].

 *)

type direction = Up | Left | Right | Down
type path = direction list

let rec big_up (zipper : Zipper.t) : Zipper.go_up_result =
  match Zipper.up zipper with
  | Top tree -> Top tree (* LOL *)
  | (Node (Here up, _) | Node (_, Here up)) -> Up (up ())
  | (Cons (Here up, _) | Cons (_, Here up)) -> big_up (up ())
  | _ -> assert false



let move_once (dir : direction) (tree : Zipper.t) : Zipper.t option =
  match dir with
  | Down ->
    (
      match Zipper.down tree with
      | Leaf -> None
      | Node (tree, _) -> Some (tree ())
      | _ -> assert false
    )
  | Up ->
    (
      match big_up tree with
      | Top _ -> None
      | Up tree -> Some tree
    )
  | Left ->
    (
      match Zipper.up tree with
      | Top _ -> None
      | Node (Here _, _) -> None
      | Cons (Here up, _) ->
        (
          match Zipper.up (up ()) with
          | Node (NotHere tree, Here _)
          | Cons (NotHere tree, Here _) -> Some (tree ())
          | _ -> assert false
        )
      | _ -> assert false
    )
  | Right ->
    (
      match Zipper.up tree with
      | Top _ -> None
      | Node (Here _, NotHere trees)
      | Cons (Here _, NotHere trees) ->
        (
          match Zipper.down (trees ()) with
          | Nil -> None
          | Cons (tree, _) -> Some (tree ())
          | _ -> assert false
        )
      | _ -> assert false
    )

let rec move path tree =
  match path with
  | [] -> Some tree
  | dir :: path -> Option.bind (move_once dir tree) (move path)

let cherry = Node (Leaf, [])

let example =
  Node (
    Leaf,
    [
      Leaf;
      Leaf;
      Node (Leaf, [Leaf]);
    ]
  )

let get = function
  | Zipper.Raw tree, _ -> tree
  | _ -> invalid_arg "get"

let () =
  let tree, ancestors =
    Zipper.zip example
    |> move [Down; Right; Right; Right; Down; Right; Up; Left; Up]
    |> Option.get
  in
  assert (ancestors = NoAncestor);
  assert (tree = Raw example)

let () =
  let ex = Zipper.zip example in
  let z1 = move [Down; Right; Right; Right; Down; Right; Up] ex in
  let z2 = move [Down; Right; Right; Right] ex in
  assert (z1 = z2)
