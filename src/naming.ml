(** {2 Naming conventions for the derivatives} *)

let ancestors = Format.sprintf "%s_ancestors"

let poly_zd = Format.sprintf "%s_poly_zd%s"

let d = Format.sprintf "%s_d%d"

let zdz = Format.sprintf "%s_zipper"
(* This type could be called %s_zdz, but it will be exposed to the user and
   therefore deserves a more human-readable name. *)
