(** Utils *)

(* ----------------------------------------------------------------------
 * Ints and int arrays
 * ---------------------------------------------------------------------- *)

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

(** Hash array of ints *)
let hash_ints a =
  let h = ref 13 in
  for i = 0 to Array.length a - 1 do
    h := (!h + 65536) * murmur_hash a.(i);
  done;
  abs !h

(* ----------------------------------------------------------------------
 * Int hashtable
 * ---------------------------------------------------------------------- *)

(** Efficient hashtable on ints. It does open addressing with linear probing. *)
module IHashtbl =
  struct
    (** A hashtable is an array of (key, value) buckets that have a state, plus the
        size of the table *)
    type 'a t = {
      mutable buckets : (int * 'a * state) array;
      mutable size : int;
    }
    and state = Used | Empty | Deleted

    (** Create a table. Size will be >= 2 *)
    let create size =
      let size = max 2 size in
      { buckets = Array.make size (0, Obj.magic None, Empty);
        size = 0; }

    (** clear the table, by resetting all states to Empty *)
    let clear t =
      for i = 0 to Array.length t.buckets - 1 do
        t.buckets.(i) <- (0, Obj.magic None, Empty)
      done;
      t.size <- 0

    (** Insert (key -> value) in buckets, starting with the hash. *)
    let insert buckets h key value =
      let n = Array.length buckets in
      (* lookup an empty slot to insert the key->value in. *)
      let rec lookup i =
        match buckets.(i) with
        | (_, _, Empty) -> buckets.(i) <- (key, value, Used)
        | (key', _, _) when key' = key -> ()
        | _ -> lookup ((i+1) mod n)
      in
      lookup (h mod n)

    (** Resize the array, by inserting its content into a twice as large array *)
    let resize buckets =
      let buckets' = Array.make (Array.length buckets * 2) (0, Obj.magic None, Empty) in
      for i = 0 to Array.length buckets - 1 do
        match buckets.(i) with
        | (key, value, Used) ->
          insert buckets' (murmur_hash key) key value  (* insert key -> value into new array *)
        | _ -> ()
      done;
      buckets'

    (** Lookup [key] in the table *)
    let find t key =
      let n = Array.length t.buckets in
      let h = murmur_hash key in
      let buckets = t.buckets in
      let rec probe i num =
        if num = n then raise Not_found
        else match buckets.(i) with
        | (key', value, Used) when key = key' ->
          value  (* found value for this key *)
        | (_, _, Deleted) | (_, _, Used) ->
          probe ((i+1) mod n) (num + 1) (* try next bucket *)
        | (_, _, Empty) -> raise Not_found
      in
      probe (h mod n) 0

    (** Maximum ratio for (number of elements / number of buckets) *)
    let max_load = 0.8

    (** put [key] -> [value] in the hashtable *)
    let replace t key value =
      (if float_of_int t.size /. float_of_int (Array.length t.buckets) > max_load then t.buckets <- resize t.buckets);
      let n = Array.length t.buckets in
      let h = murmur_hash key in
      let buckets = t.buckets in
      let rec probe i =
        match buckets.(i) with
        | (key', _, Used) when key = key' ->
          buckets.(i) <- (key, value, Used)  (* replace value *)
        | (_, _, Deleted) |(_, _, Empty) ->
          buckets.(i) <- (key, value, Used); t.size <- t.size + 1 (* insert and increment size *)
        | (_, _, Used) ->
          probe ((i+1) mod n) (* go further *)
      in
      probe (h mod n)

    (** alias for replace *)
    let add t key value = replace t key value

    (** Remove the key from the table *)
    let remove t key =
      let n = Array.length t.buckets in
      let h = murmur_hash key in
      let buckets = t.buckets in
      let rec probe i =
        match buckets.(i) with
        | (key', _, Used) when key = key' ->
          buckets.(i) <- (0, Obj.magic None, Deleted); t.size <- t.size - 1  (* remove slot *)
        | (_, _, Deleted) | (_, _, Used) ->
          probe ((i+1) mod n) (* search further *)
        | (_, _, Empty) -> ()  (* not present *)
      in
      probe (h mod n)

    (** size of the table *)
    let length t = t.size

    (** Is the key member of the table? *)
    let mem t key =
      try ignore (find t key); true
      with Not_found -> false

    (** Iterate on key -> value pairs *)
    let iter k t =
      let buckets = t.buckets in
      for i = 0 to Array.length buckets - 1 do
        match buckets.(i) with
        | (key, value, Used) -> k key value
        | _ -> ()
      done

    (** Fold on key -> value pairs *)
    let fold f t acc =
      let acc = ref acc in
      let buckets = t.buckets in
      for i = 0 to Array.length buckets - 1 do
        match buckets.(i) with
        | (key, value, Used) -> acc := f key value !acc
        | _ -> ()
      done;
      !acc
      
  end


(*
module IHashtbl = Hashtbl.Make( struct type t = int let equal i j = i = j let hash i = murmur_hash i end)
*)

(* ----------------------------------------------------------------------
 * Utils for parsing/lexing
 * ---------------------------------------------------------------------- *)

exception PARSE_ERROR

let prev_column_index = ref 0
let current_column_index = ref 0
let prev_line_index = ref 0
let current_line_index = ref 0
let current_token = ref ""
