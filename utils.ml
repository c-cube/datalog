(** Utils *)

(** Hashing on ints, cf http://en.wikipedia.org/wiki/MurmurHash *)
let murmur_hash i =
  let m = 0xd1e995
  and r = 24
  and seed = 0x47b28c in
  let hash = seed lxor 32 in
  let k = i * m in
  let k = k lxor (k lsr r) in
  let k = k * m in
  let hash = (hash * m) lxor k in
  let hash = hash lxor (hash lsr 13) in
  let hash = hash lxor (hash lsr 15) in
  abs hash

(** Efficient hashtable on ints *)
module IHashtbl = Hashtbl.Make( struct type t = int let equal i j = i = j let hash i = murmur_hash i end)

(** Sets of int *)
module ISet = Set.Make(struct type t = int let compare i j = i - j end)

(** Comparison on arrays of ints *)
let compare_ints a1 a2 =
  (* lexicographic test *)
  let rec check a1 a2 i = 
    if i = Array.length a1
      then 0
      else
        let cmp = a1.(i) - a2.(i) in
        if cmp <> 0
          then cmp
          else check a1 a2 (i+1)
  in
  if Array.length a1 <> Array.length a2
    then Array.length a1 - Array.length a2
    else check a1 a2 0

(** Set of arrays of ints *)
module IASet = Set.Make(struct type t = int array let compare = compare_ints end)
