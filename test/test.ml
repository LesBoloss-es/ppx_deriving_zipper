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

let za = zip_tree a

let () = assert (za = (a, []))

let () = print_endline "test.ml OK"
