type tree =
  | Nil
  | UNode of float * tree
  | BNode of int * tree * tree
[@@deriving zipper]

let a =
  BNode (
    7,
    UNode (
      2.5,
      UNode (
        7.8,
        Nil
      )
    ),
    BNode (
      4,
      Nil,
      UNode (
        2.7,
        BNode (
          89,
          Nil,
          Nil
        )
      )
    )
  )


let () =
  let rec go_bot_left z = match view_tree z with
    | ZNil -> z
    | ZUNode (f, child) ->
      Format.printf "%F@." f;
      go_bot_left (child ())
    | ZBNode (n, left, _) ->
      Format.printf "%d@." n;
      go_bot_left (left ())
  in
  let z = go_bot_left (zip_tree a) in
  ignore z

let () = print_endline "test.ml OK"
