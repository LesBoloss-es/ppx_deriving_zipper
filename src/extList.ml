let rec map_filter f = function
  | [] -> []
  | h :: t ->
    match f h with
    | None -> map_filter f t
    | Some x -> x :: map_filter f t

let find_all_indices p =
  let rec find_all_indices i = function
    | [] -> []
    | h :: t when p h -> i :: find_all_indices (i+1) t
    | _ :: t -> find_all_indices (i+1) t
  in
  find_all_indices 0

let rec replace_nth l i v =
  match i, l with
  | _, [] -> failwith "ExtList.replace_nth"
  | 0, _ :: t -> v :: t
  | _, h :: t -> h :: replace_nth t (i-1) v
