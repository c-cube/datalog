(** A bijective mapping between strings and (positive) integers *)

val mk_symbol : string -> int
  (** Internalize string into a symbol *)

val get_symbol : int -> string
  (** Get the string associated with the int *)

val rm_symbol : string -> unit
  (** Forget about the symbol. If the corresponding int [i] it still used,
      [get_symbol i] will fail with Not_found. *)

