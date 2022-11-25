(lang dune 2.5)
(generate_opam_files true)

(package
 (name ppx_deriving_zipper)
 (version 0.1)

 (synopsis "A PPX deriver for zippers")
 (tags (syntax))

 (source (github LesBoloss-es/ppx_deriving_zipper))

 (license LGPL-3.0-or-later)
 (authors
   "Kerl <kerl@wkerl.me>"
   "Niols <niols@niols.fr>")
 (maintainers
   "Kerl <kerl@wkerl.me>"
   "Niols <niols@niols.fr>")

 (depends
  (dune :build)
  (ocaml :build)
   ppx_deriving
   ppxlib
  (ppx_inline_test (or :build :with-test))))
