(** A bijective mapping between strings and (positive) integers *)

let s_to_i = Hashtbl.create 5
let i_to_s = Utils.IHashtbl.create 5
let count = ref 0

(** Internalize string into a symbol *)
let mk_symbol s =
  try Hashtbl.find s_to_i s
  with Not_found ->
    let i = !count in
    incr count;
    Hashtbl.replace s_to_i s i;
    Utils.IHashtbl.replace i_to_s i s;
    i

(** Get the string associated with the int *)
let get_symbol i = Utils.IHashtbl.find i_to_s i

(** Forget about the symbol. If the corresponding int [i] it still used,
    [get_symbol i] will fail with Not_found. *)
let rm_symbol s =
  try
    let i = Hashtbl.find s_to_i s in
    Utils.IHashtbl.remove i_to_s i;
    Hashtbl.remove s_to_i s
  with Not_found -> ()

