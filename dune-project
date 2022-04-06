(lang dune 2.5)
(generate_opam_files true)

(package
 (name ppx_deriving_zipper)
 (version 0.1)

 (synopsis "A PPX deriver for zippers")
 (tags (syntax))


 (license LGPL3)
 (authors
   "Kerl <kerl@wkerl.me>"
   "Niols <niols@niols.fr>")

 (depends
  (dune :build)
  (ocaml :build)
   ppx_deriving
   ppxlib
  (ppxfind :build)))
