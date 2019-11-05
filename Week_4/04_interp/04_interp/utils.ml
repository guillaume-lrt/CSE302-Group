module StringMap = Map.Make(String)
module StringTab =
  Hashtbl.Make(struct
    type t = string
    let equal (x : t) y = x = y
    let hash (x : t) = Hashtbl.hash x
  end)

let pp_commaspace ff () =
  Format.(pp_print_string ff "," ; pp_print_space ff ())

let debug fmt = Format.fprintf Format.err_formatter ("[DEBUG] " ^^ fmt)

let failwithf fmt = Format.kasprintf failwith fmt
