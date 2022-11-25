`ppx_deriving_zipper`
=====================

Two test directories:

- `test/` contains examples of type definitions and a Shell script able to call 
  the PPX on those types. Try, for instance:
  ```sh
  test/standalone.sh test/bintree_list.ml
  ```

- `examples/` contains examples of types and their *hand-written* PPXes. It also
  contains some assertions that help with tests. Try, for instance:
  ```sh
  dune exec examples/tree_list.exe
  ```
