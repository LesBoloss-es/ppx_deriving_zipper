open Ppxlib

(** {1 Our AST Helpers} *)

let str txt = Location.{ txt; loc = none }

let lid s =
  Location.{ txt = Longident.parse s; loc = none }
