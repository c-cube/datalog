(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Utils} *)

(* ----------------------------------------------------------------------
 * Ints and int arrays
 * ---------------------------------------------------------------------- *)

(** Hashing on ints, cf http://en.wikipedia.org/wiki/MurmurHash *)
let murmur_hash i =
  let m = 0x1bd1e995 in  (* 0x5bd1e995 in 31 bits *)
  let r = 24 in
  let seed = 0x1747b28c in  (* 0x9747b28c in 31 bits *)
  let hash = seed lxor 4 in
  let k = i * m in
  let k = k lxor (k lsr r) in
  let k = k * m in
  let hash = (hash * m) lxor k in
  let hash = hash lxor (hash lsr 13) in
  let hash = hash * m in
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
      mutable buckets : 'a bucket array;
      mutable size : int;
    }
    and 'a bucket = Empty | Deleted of int | Used of (int * 'a)

    (** Create a table. Size will be >= 2 *)
    let create size =
      let size = max 2 size in
      { buckets = Array.make size Empty;
        size = 0; }

    (** clear the table, by resetting all states to Empty *)
    let clear t =
      Array.fill t.buckets 0 (Array.length t.buckets) Empty;
      t.size <- 0

    (** Insert (key -> value) in buckets, starting with the hash. *)
    let insert buckets h key value =
      let n = Array.length buckets in
      (* lookup an empty slot to insert the key->value in. *)
      let rec lookup i =
        match buckets.(i) with
        | Empty -> buckets.(i) <- Used (key, value)
        | Used (key', _) | Deleted key'  when key' = key -> ()
        | _ -> lookup ((i+1) mod n)
      in
      lookup (h mod n)

    (** Resize the array, by inserting its content into a twice as large array *)
    let resize buckets =
      let buckets' = Array.make (Array.length buckets * 2) Empty in
      for i = 0 to Array.length buckets - 1 do
        match buckets.(i) with
        | Used (key, value) ->
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
        | Used (key', value) when key = key' ->
          value  (* found value for this key *)
        | Deleted _ | Used _ ->
          probe ((i+1) mod n) (num + 1) (* try next bucket *)
        | Empty -> raise Not_found
      in
      probe (h mod n) 0

    (** Maximum ratio for (number of elements / number of buckets) *)
    let max_load = 0.8

    (** put [key] -> [value] in the hashtable *)
    let replace t key value =
      let load = float_of_int t.size /. float_of_int (Array.length t.buckets) in
      (if load > max_load then t.buckets <- resize t.buckets);
      let n = Array.length t.buckets in
      let h = murmur_hash key in
      let buckets = t.buckets in
      let rec probe i =
        match buckets.(i) with
        | Used (key', _) when key = key' ->
          buckets.(i) <- Used (key, value)  (* replace value *)
        | Deleted _ | Empty ->
          buckets.(i) <- Used (key, value);
          t.size <- t.size + 1 (* insert and increment size *)
        | Used (_, _) ->
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
        | Used (key', _) when key = key' ->
          buckets.(i) <- Deleted key; t.size <- t.size - 1  (* remove slot *)
        | Deleted _ | Used _ ->
          probe ((i+1) mod n) (* search further *)
        | Empty -> ()  (* not present *)
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
        | Used (key, value) -> k key value
        | _ -> ()
      done

    (** Fold on key -> value pairs *)
    let fold f t acc =
      let acc = ref acc in
      let buckets = t.buckets in
      for i = 0 to Array.length buckets - 1 do
        match buckets.(i) with
        | Used (key, value) -> acc := f key value !acc
        | _ -> ()
      done;
      !acc
  end

(*
module IHashtbl = Hashtbl.Make( struct type t = int let equal i j = i = j let hash i = murmur_hash i end)
*)

