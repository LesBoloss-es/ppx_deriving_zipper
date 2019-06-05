type tree =
  | Nil
  | UNode of float * tree
  | BNode of (int * tree) * tree
[@@deriving zipper]

let rec pp_tree fmt = function
  | Nil -> Format.fprintf fmt "Nil"
  | UNode (f, t) -> Format.fprintf fmt "@[<2>UNode(%F,@ %a)@]" f pp_tree t
  | BNode ((n, l), r) -> Format.fprintf fmt "@[<2>BNode((%d,@ %a),@ %a)@]" n pp_tree l pp_tree r

let my_tree =
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

let rec go_bot_left z =
  match TreeZipper.view z with
  | ZNil -> z
  | ZUNode (f, child) ->
    Format.printf "%F@." f;
    go_bot_left (child ())
  | ZBNode ((n, left), _) ->
    Format.printf "%d@." n;
    go_bot_left (left ())

let my_tree =
  let (_, ancestors) = go_bot_left (TreeZipper.zip my_tree) in
  TreeZipper.unzip (UNode (42.42, Nil), ancestors)

let () = Format.printf "%a@." pp_tree my_tree

let () =
  let (a, b, c) =
    TreeZipper.fold_left
      (fun (a, b, c) -> function
         | Nil ->
           Format.printf "Nil@.";
           (a+1, b, c)
         | UNode (f, _) ->
           Format.printf "UNode (%F, _)@." f;
           (a, b+1, c)
         | BNode ((i, _), _) ->
           Format.printf "BNode ((%d, _), _)@." i;
           (a, b, c+1))
      (0, 0, 0)
      my_tree
  in
  Format.printf "#Nil = %d ; #UNode = %d ; #BNode = %d@." a b c

let () = print_endline "test.ml out@."
