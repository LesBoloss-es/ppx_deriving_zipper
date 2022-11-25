`tree_list`
===========

Our running example: a tree that is either a `Leaf` or a `Node` of a non-zero
number of children, encoded as:

``` ocaml
type tree =
  | Leaf
  | Node of tree * tree list
```

This is complex enough that it allows us to check a number of things. For ease
of work, we use our custom `lst` type:

``` ocaml
type 'a lst = Nil | Cons of 'a * 'a lst 
```
