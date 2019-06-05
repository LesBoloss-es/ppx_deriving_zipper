type tree =
  | Nil
  | UNode of float * tree
  | BNode of (int * tree) * tree
[@@deriving zipper]

let rec pp_tree fmt = function
  | Nil -> Format.fprintf fmt "Nil"
  | UNode (f, t) -> Format.fprintf fmt "@[<2>UNode(%F,@ %a)@]" f pp_tree t
  | BNode ((n, l), r) -> Format.fprintf fmt "@[<2>BNode((%d,@ %a),@ %a)@]" n pp_tree l pp_tree r

let a =
  BNode (
    (7,
     UNode (
       2.5,
       UNode (
         7.8,
         Nil
       )
     )),
    BNode (
      (4,
       Nil),
      UNode (
        2.7,
        BNode (
          (89,
           Nil),
          Nil
        )
      )
    )
  )

let () =
  let rec go_bot_left z =
    match TreeZipper.view z with
    | ZNil -> z
    | ZUNode (f, child) ->
      Format.printf "%F@." f;
      go_bot_left (child ())
    | ZBNode ((n, left), _) ->
      Format.printf "%d@." n;
      go_bot_left (left ())
  in
  let (_, ancestors) = go_bot_left (TreeZipper.zip a) in
  let tree = TreeZipper.unzip (UNode (42.42, Nil), ancestors) in
  Format.printf "%a@." pp_tree tree

let () = print_endline "test.ml OK"
