(** {1 General zipper utilities} *)

type hole = Hole
(** We see a zipper, aka a data structure with a distinguished element, as a
    pair of a structure with a hole in place of that distinguished element,
    and that element on the side.
    There is no need to physically represent this holes in the type of zippers
    but this helps readability.
    This is the type of holes. *)

(** Result of a [go_up] operation: either already at the [Top], returning the
    unzipped zipper, or went [Up] once. *)
type ('typ, 'zipper_type) go_up_result =
  | Top of 'typ
  | Up of 'zipper_type


(** {2 Pretty printing} *)

let pp_hole fmt Hole =
  Format.pp_print_string fmt "□"
