(** Representation for Datalog terms and rules *)

(* ----------------------------------------------------------------------
 * Terms and rules
 * ---------------------------------------------------------------------- *)

type term = int array
  (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
      array is the predicate, then arguments follow *)

type rule = term array
  (** A datalog rule, i.e. head :- body_1, ..., body_n *)

type subst = int Utils.IHashtbl.t
  (** A substitution is a map from (negative) ints to (positive) ints *)

let is_var x = x < 0

(** Is the term ground (a fact)? *)
let is_ground t =
  assert (not (is_var t.(0)));
  let rec check t i =
    if i = Array.length t then true
    else (not (is_var t.(i))) && check t (i+1)
  in
  check t 1

(** Number of subterms of the term. Ex for p(a,b,c) it returns 3 *)
let arity t = Array.length t - 1

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

(** Apply substitution to the term *)
let subst_term subst t =
  if is_ground t
    then t
    else begin
      (* replace variables in a copy of t by their value in the subst *)
      let a = Array.copy t in
      for i = 1 to Array.length t - 1 do
        if is_var a.(i) then
          a.(i) <- try Utils.IHashtbl.find subst a.(i)
                   with Not_found -> a.(i)
      done;
      a
    end

(** Apply substitution to the rule. TODO remove duplicate literals afterward *)
let subst_rule subst rule =
  let a = Array.copy rule in
  for i = 0 to Array.length rule - 1 do
    a.(i) <- subst_term subst a.(i);
  done;
  a

(** A datalog rule is safe iff all variables in its head also occur in its body *)
let check_safe rule =
  let head_vars = vars ~start:0 ~stop:0 rule in
  let body_vars = vars ~start:1 rule in
  Utils.ISet.for_all (fun x -> Utils.ISet.mem x body_vars) head_vars

let pp_term to_s formatter t =
  if arity t = 0
    then Format.fprintf formatter "%s" (to_s t.(0))
    else begin
      Format.fprintf formatter "%s(" (to_s t.(0));
      for i = 1 to Array.length t - 1 do
        (if i > 1 then Format.fprintf formatter ", ");
        Format.fprintf formatter "%s" (to_s t.(i));
      done;
      Format.fprintf formatter ")";
    end

let pp_rule to_s formatter rule =
  if Array.length rule = 1
    then Format.fprintf formatter "%a." (pp_term to_s) rule.(0)
    else begin
      Format.fprintf formatter "%a :-@ "  (pp_term to_s) rule.(0);
      for i = 1 to Array.length rule - 1 do
        (if i > 1 then Format.fprintf formatter ",@ ");
        Format.fprintf formatter "%a" (pp_term to_s) rule.(i);
      done;
      Format.fprintf formatter ".";
    end

(* ----------------------------------------------------------------------
 * Generalization index on terms
 * ---------------------------------------------------------------------- *)
