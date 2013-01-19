(** bitvectors of at most 31 bits *)

(** a bitvector is just an int *)
type t = int

(** make a bitvector of size n with all bits set *)
let bv_make n =
  assert (n <= 31);
  let rec shift bv n = if n = 0 then bv else shift ((bv lsl 1) lor 1) (n-1)
  in shift 0 n

(** bitvector n-th element is true? *)
let bv_get bv n = (bv land (1 lsl n)) <> 0 

(** set n-th element of bitvector *)
let bv_set bv n = bv lor (1 lsl n)

(** reset n-th element of bitvector *)
let bv_clear bv n = bv land (lnot (1 lsl n))

(** is bitvector empty? *)
let bv_empty bv = bv = 0

(** shift bitvector to right *)
let bv_lsr bv n = bv lsr n
