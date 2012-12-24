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

(** Are the terms equal? *)
let eq_term t1 t2 = Utils.compare_ints t1 t2 = 0

(** Hash the term *)
let hash_term t = Utils.hash_ints t

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

(** Check whether rules are (syntactically) equal *)
let eq_rule r1 r2 =
  let rec check r1 r2 i =
    if i = Array.length r1 then true else
    eq_term r1.(i) r2.(i) && check r1 r2 (i+1)
  in
  Array.length r1 = Array.length r2 && check r1 r2 0

(** Hash the rule *)
let hash_rule r =
  let h = ref 17 in
  for i = 0 to Array.length r - 1 do
    h := (!h + 65536) * hash_term r.(i);
  done;
  abs !h

let pp_term ?(to_s=Symbols.get_symbol) formatter t =
  if arity t = 0
    then Format.fprintf formatter "%s" (to_s t.(0))
    else begin
      Format.fprintf formatter "%s(" (to_s t.(0));
      for i = 1 to Array.length t - 1 do
        (if i > 1 then Format.fprintf formatter ", ");
        if is_var t.(i)
          then Format.fprintf formatter "X%d" (abs t.(i))
          else Format.fprintf formatter "%s" (to_s t.(i));
      done;
      Format.fprintf formatter ")";
    end

let pp_rule ?(to_s=Symbols.get_symbol) formatter rule =
  if Array.length rule = 1
    then Format.fprintf formatter "%a." (pp_term ~to_s) rule.(0)
    else begin
      Format.fprintf formatter "%a :-@ "  (pp_term ~to_s) rule.(0);
      for i = 1 to Array.length rule - 1 do
        (if i > 1 then Format.fprintf formatter ",@ ");
        Format.fprintf formatter "%a" (pp_term ~to_s) rule.(i);
      done;
      Format.fprintf formatter ".";
    end

(* ----------------------------------------------------------------------
 * Generalization/Specialization index on terms
 * ---------------------------------------------------------------------- *)

(** Hashtable on terms *)
module TermHashtbl = Hashtbl.Make(
  struct
    type t = term
    let equal = eq_term
    let hash = Utils.hash_ints
  end)

(** Type for an indexing structure on terms *)
module type Index =
  sig
    type t
      (** A term index *)

    type elt
      (** A value indexed by a term *)

    val create : unit -> t
      (** Create a new index *)

    val add : t -> term -> elt -> unit
      (** Add an element indexed by the term *)

    val clear : t -> unit
      (** Reset to empty index *)

    val retrieve_generalizations : ('a -> elt -> subst -> 'a) -> 'a -> t -> term -> 'a
      (** Fold on generalizations of given term (with transient substitution) *)

    val retrieve_specializations : ('a -> elt -> subst -> 'a) -> 'a -> t -> term -> 'a
      (** Fold on specifications of given term (with transient substitution) *)

    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
      (** Fold on all indexed elements *)

    val is_empty : t -> bool
      (** Is the index empty? *)

    val size : t -> int
      (** Number of indexed elements (linear) *)
  end

(** Create an Index module for the given type of elements. The implementation
    is based on perfect discrimination trees. *)
module Make(H : Hashtbl.HashedType) : Index with type elt = H.t =
  struct

    (** A hashtable on indexed data *)
    module DataHashtbl = Hashtbl.Make(H)

    (** The term index. It is a trie with, at each node, a hashset
        of elements, plus a map symbol/var -> subtrie *)
    type t = 
    | Node of unit DataHashtbl.t * t Utils.IHashtbl.t  

    (** Indexed elements *)
    type elt = H.t

    (** Create a new index *)
    let create () = Node (DataHashtbl.create 3, Utils.IHashtbl.create 3)

    (** Add the element indexed by the term *)
    let add t term elt =
      let len = Array.length term in
      (* index in subtrie [t], with a cursor at term[i]. *)
      let rec add t i = match t, i with
      | Node (set, subtries), i when i = len ->
        DataHashtbl.replace set elt ()  (* insert in leaf *)
      | Node (_, subtries), i ->
        try
          let subtrie = Utils.IHashtbl.find subtries term.(i) in
          add subtrie (i+1)
        with Not_found ->
          (* create a new subtrie for the i-th argument of term, then recurse *)
          let subtrie = Node (DataHashtbl.create 3, Utils.IHashtbl.create 3) in
          Utils.IHashtbl.add subtries term.(i) subtrie;
          add subtrie (i+1)
      in
      add t 0

    (** Reset to empty index *)
    let clear t = match t with
      | Node (set, subtries) ->
        DataHashtbl.clear set;
        Utils.IHashtbl.clear subtries

    (** Fold on generalizations of given ground term (with transient substitution) *)
    let retrieve_generalizations k acc t term =
      assert (is_ground term);
      let subst = Utils.IHashtbl.create 3 in
      let len = Array.length term in
      (* search in subtrie [t], with cursor at [i]-th argument of [term] *)
      let rec search t i acc = match t, i with
      | Node (set, _), i when i = len ->
        DataHashtbl.fold (fun elt _ acc -> k acc elt subst) set acc
      | Node (_, subtries), i ->
        let sym = term.(i) in
        Utils.IHashtbl.fold
          (fun sym' subtrie acc ->
            if is_var sym'
            then (* try to bind variable to current symbol *)
              try
                (* var already bound, continue iff bound to same symbol *)
                if Utils.IHashtbl.find subst sym' = sym
                  then search subtrie (i+1) acc
                  else acc
              with Not_found ->
                (* bind sym' to sym and recurse *)
                Utils.IHashtbl.add subst sym' sym;
                let acc' = search subtrie (i+1) acc in
                Utils.IHashtbl.remove subst sym';
                acc'
            else if sym = sym'
              (* same symbol, go in the subtrie *)
              then search subtrie (i+1) acc
            else acc)
          subtries acc
      in
      search t 0 acc

    (** Fold on ground specifications of given term (with transient substitution) *)
    let retrieve_specializations k acc t term =
      let subst = Utils.IHashtbl.create 3 in
      let len = Array.length term in
      (* search in subtrie [t], with cursor at [i]-th argument of [term] *)
      let rec search t i acc = match t, i with
      | Node (set, _), i when i = len ->
        DataHashtbl.fold (fun elt _ acc -> k acc elt subst) set acc
      | Node (_, subtries), i when is_var term.(i) ->
        let var = term.(i) in
        (try
          (* t[i] = var that is already bound, follow the corresponding branch, if any *)
          let sym' = Utils.IHashtbl.find subst var in
          try
            let subtrie = Utils.IHashtbl.find subtries sym' in
            search subtrie (i+1) acc
          with Not_found -> acc
        with Not_found ->
          (* follow every ground branch, binding variable *)
          Utils.IHashtbl.fold
            (fun sym subtrie acc ->
              (* only follow branches leading to ground terms *)
              if is_var sym then acc else begin
                (* bind var to sym and recurse *)
                Utils.IHashtbl.add subst var sym;
                let acc' = search subtrie (i+1) acc in
                Utils.IHashtbl.remove subst var;
                acc'
              end)
          subtries acc)
      | Node (_, subtries), i ->
        (* just follow the corresponding branch, if there is one *)
        try
          let subtrie = Utils.IHashtbl.find subtries term.(i) in
          search subtrie (i+1) acc
        with Not_found -> acc
      in
      search t 0 acc

    (** Fold on all indexed elements *)
    let rec fold k acc t = match t with
      | Node (set, subtries) ->
        (* fold on elements at this point *)
        let acc = DataHashtbl.fold
          (fun elt () acc -> k acc elt) set acc in
        (* fold on subtries *)
        Utils.IHashtbl.fold
          (fun _ subtrie acc -> fold k acc subtrie)
          subtries acc

    (** Check whether the property is true for all subtries *)
    let for_all p subtries =
      try
        Utils.IHashtbl.iter
          (fun _ t' -> if not (p t') then raise Exit)
        subtries;
        true
      with Exit -> false

    (** Check whether there are no elements in the index *)
    let rec is_empty t = match t with
      | Node (set, subtries) ->
        DataHashtbl.length set = 0 && for_all is_empty subtries

    (** Number of elements *)
    let size t = fold (fun i _ -> i + 1) 0 t
  end
