(** Representation for Datalog terms and rules *)

(* ----------------------------------------------------------------------
 * Terms and rules
 * ---------------------------------------------------------------------- *)

type term = int array
  (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
      array is the predicate, then arguments follow *)

type rule = term array
  (** A datalog rule, i.e. head :- body_1, ..., body_n *)

let is_var x = x < 0

(** Set of variables of the terms *)
let vars ?start ?stop terms =
  let acc = ref Utils.ISet.empty in
  (* range of elements of the array to consider *)
  let start, stop = match start, stop with
  | Some i, Some j -> i, j
  | Some i, None -> i, Array.length terms - 1
  | None, Some j -> 0, j
  | None, None -> 0, Array.length terms - 1
  in
  (* explore the terms *)
  Array.iteri
    (fun i t ->
      if start <= i && i <= stop
        then let t = terms.(i) in
        for j = 1 to Array.length t - 1 do
          let x = t.(j) in
          if is_var x then acc := Utils.ISet.add x !acc;
        done)
    terms;
  !acc

(** A datalog rule is safe iff all variables in its head also occur in its body *)
let check_safe rule =
  let head_vars = vars ~start:0 ~stop:0 rule in
  let body_vars = vars ~start:1 rule in
  Utils.ISet.for_all (fun x -> Utils.ISet.mem x body_vars) head_vars

(* ----------------------------------------------------------------------
 * Generalization index on terms
 * ---------------------------------------------------------------------- *)
