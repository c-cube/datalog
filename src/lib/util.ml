
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

(** {1 Various Utils} *)

let combine_hash hash i =
  abs (hash * 65599 + i)

let rec hash_list f h l = match l with
  | [] -> h
  | x::l' -> hash_list f (combine_hash h (f x)) l'

let rec list_eq eq l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | x1::l1', x2::l2' -> eq x1 x2 && list_eq eq l1' l2'

let hash_array f h a =
  let h = ref h in
  Array.iter (fun x -> h := combine_hash !h (f x)) a;
  !h

let array_eq eq a b =
  Array.length a = Array.length b &&
  begin try
    for i = 0 to Array.length a - 1 do
      if not (eq a.(i) b.(i)) then raise Exit;
    done;
    true
  with Exit -> false
  end

let array_forall2 p a1 a2 =
  if Array.length a1 = Array.length a2
    then try
      for i = 0 to Array.length a1 - 1 do
        if not (p a1.(i) a2.(i)) then raise Exit
      done;
      true
    with Exit -> false
    else false

let array_exists p a =
  try
    for i = 0 to Array.length a - 1 do
      if p a.(i) then raise Exit
    done;
    false
  with Exit -> true

let array_fold2 f acc a1 a2 =
  if Array.length a1 <> Array.length a2
    then failwith "_array_fold2: arrays must have same length";
  let acc = ref acc in
  for i = 0 to Array.length a1 - 1 do
    acc := f !acc a1.(i) a2.(i)
  done;
  !acc

