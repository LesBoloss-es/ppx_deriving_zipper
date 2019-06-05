type tree =
  | Nil
  | UNode of int * tree
  | BNode of float * tree * tree
  | TNode of string * tree * tree * tree
[@@deriving
  visitors { variety = "iter";   name = "visitor_iter_tree" },
  visitors { variety = "reduce"; name = "visitor_reduce_tree" },
  zipper]

let generate_tree () = (* FIXME: please change me :-( *)
  let a = Nil in
  let a = UNode (1, a) in
  let a = BNode (2., a, a) in
  let a = TNode ("3", a, a, a) in
  a

type 'a poly_tree =
  | PNil of 'a
  | PUNode of 'a * int * 'a poly_tree
  | PBNode of 'a * float * 'a poly_tree * 'a poly_tree
  | PTNode of 'a * string * 'a poly_tree * 'a poly_tree * 'a poly_tree
[@@deriving
  visitors { variety = "iter";   name = "visitor_iter_poly_tree" },
  visitors { variety = "reduce"; name = "visitor_reduce_poly_tree" },
  iter, fold]

let generate_poly_tree () =
  let a = PNil () in
  let a = PUNode ((), 1, a) in
  let a = PBNode ((), 2., a, a) in
  let a = PTNode ((), "3", a, a, a) in
  a

let deriving_iter_poly_tree t =
  let n = ref 0 in
  iter_poly_tree (fun _ -> incr n) t;
  !n

let deriving_fold_poly_tree t =
  fold_poly_tree (fun n _ -> n + 1) 0 t

let visitor_iter_tree =
  let v = object
    val mutable n = 0
    method n = n
    inherit [_] visitor_iter_tree as super
    method! visit_tree () t =
      n <- n + 1;
      super#visit_tree () t
  end in
  fun t ->
    v#visit_tree () t;
    v#n

let visitor_iter_poly_tree =
  let v = object
    val mutable n = 0
    method n = n
    inherit [_] visitor_iter_poly_tree as super
    method visit_'a () () = ()
    method! visit_poly_tree () t =
      n <- n + 1;
      super#visit_poly_tree () t
  end in
  fun t ->
    v#visit_poly_tree () t;
    v#n

let visitor_reduce_tree =
  let v = object
    inherit [_] visitor_reduce_tree

    method zero = 1
    method plus = (+)
  end in
  fun t ->
    v#visit_tree () t

let visitor_reduce_poly_tree =
  let v = object
    inherit [_] visitor_reduce_poly_tree

    method zero = 0
    method plus = (+)

    method visit_'a () () = 1
  end in
  fun t ->
    v#visit_poly_tree () t

let zipper_fold t =
  TreeZipper.fold_left (fun n _ -> n + 1) 0 t

let nb = 10000000
let trees = Array.init nb (fun _ -> generate_tree ())
let poly_trees = Array.init nb (fun _ -> generate_poly_tree ())

let () =
  let start = Sys.time () in
  Array.iter
    (fun poly_tree ->
       ignore (deriving_iter_poly_tree poly_tree))
    poly_trees;
  let end_ = Sys.time () in
  Format.eprintf "| deriving | iter   | poly tree | %f |@." (end_ -. start)

let () =
  let start = Sys.time () in
  Array.iter
    (fun poly_tree ->
       ignore (deriving_fold_poly_tree poly_tree))
    poly_trees;
  let end_ = Sys.time () in
  Format.eprintf "| deriving | fold   | poly tree | %f |@." (end_ -. start)

let () =
  let start = Sys.time () in
  Array.iter
    (fun tree ->
       ignore (visitor_iter_tree tree))
    trees;
  let end_ = Sys.time () in
  Format.eprintf "| visitors | iter   |      tree | %f |@." (end_ -. start)

let () =
  let start = Sys.time () in
  Array.iter
    (fun poly_tree ->
       ignore (visitor_iter_poly_tree poly_tree))
    poly_trees;
  let end_ = Sys.time () in
  Format.eprintf "| visitors | iter   | poly tree | %f |@." (end_ -. start)

let () =
  let start = Sys.time () in
  Array.iter
    (fun tree ->
       ignore (visitor_reduce_tree tree))
    trees;
  let end_ = Sys.time () in
  Format.eprintf "| visitors | reduce |      tree | %f |@." (end_ -. start)

let () =
  let start = Sys.time () in
  Array.iter
    (fun poly_tree ->
       ignore (visitor_reduce_poly_tree poly_tree))
    poly_trees;
  let end_ = Sys.time () in
  Format.eprintf "| visitors | reduce | poly tree | %f |@." (end_ -. start)

let () =
  let start = Sys.time () in
  Array.iter
    (fun tree ->
       ignore (zipper_fold tree))
    trees;
  let end_ = Sys.time () in
  Format.eprintf "| zipper   | fold   |      tree | %f |@." (end_ -. start)
