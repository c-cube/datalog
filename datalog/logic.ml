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

(** Representation for Datalog literals and clauses, and the main algorithm *)

(** Module type for logic *)
module type S = sig
  (** {2 Literals and clauses} *)

  type symbol
    (** Abstract type of symbols *)

  type literal
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
        array is the predicate, then arguments follow *)

  type clause
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  type subst
    (** A substitution maps variables to symbols *)

  (** {3 Constructors and destructors} *)

  val mk_literal : symbol -> [`Var of int | `Symbol of symbol] list -> literal
    (** Helper to build a literal. Arguments are either variables or symbols; if they
        variables indexes *must* be negative (otherwise it will raise Invalid_argument *)

  val mk_literal_s : string -> [`Var of int | `Symbol of string] list -> literal
    (** Same as [mk_literal], but converts strings to symbols on-the-fly *)

  val open_literal : literal -> symbol * [`Var of int | `Symbol of symbol] list
    (** Deconstruct a literal *)

  val mk_clause : literal -> literal list -> clause
    (** Create a clause from a conclusion and a list of premises *)

  val open_clause : clause -> literal * literal list
    (** Deconstruct a clause *)

  val is_var : int -> bool
    (** A variable is a negative int *)

  val is_ground : literal -> bool
    (** Is the literal ground (a fact)? *)

  val arity : literal -> int
    (** Number of subliterals of the literal. Ex for p(a,b,c) it returns 3 *)

  (** {3 Comparisons} *)

  val eq_literal : literal -> literal -> bool
    (** Are the literals equal? *)

  val hash_literal : literal -> int
    (** Hash the literal *)

  val compare_literal : literal -> literal -> int
    (** Arbitrary comparison of literals (lexicographic) *)

  val check_safe : clause -> bool
    (** A datalog clause is safe iff all variables in its head also occur in its body *)

  val is_fact : clause -> bool
    (** A fact is a ground clause with empty body *)

  val compare_clause : clause -> clause -> int
    (** Lexicographic comparison of clauses *)

  val eq_clause : clause -> clause -> bool
    (** Check whether clauses are (syntactically) equal *)

  val hash_clause : clause -> int
    (** Hash the clause *)

  (** {3 Comparisons} *)

  val subst_literal : subst -> literal -> literal
    (** Apply substitution to the literal *)

  val subst_clause : subst -> clause -> clause
    (** Apply substitution to the clause *)

  (** {3 Pretty-printing} *)

  val pp_literal : Format.formatter -> literal -> unit
    (** Pretty print the literal *)

  val pp_clause : Format.formatter -> clause -> unit
    (** Pretty print the clause *)

  val pp_subst : Format.formatter -> subst -> unit
    (** Pretty print the substitution *)

  (** {2 The Datalog unit resolution algorithm} *)

  exception UnsafeClause

  type db
    (** A database of facts and clauses, with incremental fixpoint computation *)

  val db_create : unit -> db
    (** Create a DB *)

  val db_mem : db -> clause -> bool
    (** Is the clause member of the DB? *)

  val db_add : db -> clause -> unit
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val db_match : db -> literal -> (literal -> subst -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match (with the corresponding substitution) *)

  val db_size : db -> int
    (** Size of the DB *)

  val db_fold : ('a -> clause -> 'a) -> 'a -> db -> 'a
    (** Fold on all clauses in the current DB (including fixpoint) *)

  val db_subscribe : db -> symbol -> (literal -> unit) -> unit
    (** [db_subscribe db symbol handler] causes [handler] to be called with
        any new fact that has head symbol [symbol] from now on *)

  val db_explain : db -> literal -> literal list
    (** Explain the given fact by returning a list of facts that imply it
        under the current clauses. *)

  val db_premises : db -> literal -> clause * literal list
    (** Immediate premises of the fact (ie the facts that resolved with
        a clause to give the literal), plus the clause that has been used. *)
end

(** Signature for a symbol type. It must be hashable, comparable and
    in bijection with strings *)
module type SymbolType = sig
  include Hashtbl.HashedType
  val to_string : t -> string
  val of_string : string -> t
    val lock : unit -> unit
    val unlock : unit -> unit
end

module Make(Symbol : SymbolType) = struct
  (* ----------------------------------------------------------------------
   * Literals and clauses
   * ---------------------------------------------------------------------- *)

  type symbol = Symbol.t
    (** Abstract type of symbols *)

  module SymbolHashtbl = Hashtbl.Make(Symbol)

  (* bijective mapping int <-> symbols *)
  let __i_to_s = Utils.IHashtbl.create 5
  let __s_to_i = SymbolHashtbl.create 5
  let __symbol_count = ref 0

  (** Perform the operation in a locked context *)
  let with_lock k =
    Symbol.lock ();
    try
      let y = k () in
      Symbol.unlock ();
      y
    with e ->
      Symbol.unlock ();
      raise e

  (** Convert a symbol to an integer *)
  let s_to_i s =
    try SymbolHashtbl.find __s_to_i s
    with Not_found ->
      let i = !__symbol_count in
      incr __symbol_count;
      SymbolHashtbl.replace __s_to_i s i;
      Utils.IHashtbl.replace __i_to_s i s;
      i

  (** Convert an integer back to a symbol *)
  let i_to_s i =
    try Utils.IHashtbl.find __i_to_s i
    with Not_found ->
      Symbol.unlock ();
      raise Not_found

  (** Forget about the symbol. If the corresponding int [i] it still used,
      [get_symbol i] will fail with Not_found. *)
  let rm_symbol s =
    try
      let i = SymbolHashtbl.find __s_to_i s in
      Utils.IHashtbl.remove __i_to_s i;
      SymbolHashtbl.remove __s_to_i s
    with Not_found -> ()

  type literal = int array
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
        array is the predicate, then arguments follow *)

  type clause = literal array
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  type subst = int Utils.IHashtbl.t
    (** A substitution is a map from (negative) ints to (positive) ints *)

  (** Helper to build a literal. Arguments are either variables or symbols; if they
      are variables, the int must be negative. *)
  let mk_literal head args =
    let head = s_to_i head in
    Symbol.lock ();
    let args = List.map
      (function
       | `Var i -> assert (i < 0); i
       | `Symbol s -> s_to_i s)
      args in
    Symbol.unlock ();
    Array.of_list (head :: args)

  (** Same as [mk_literal], but converts strings to symbols on-the-fly *)
  let mk_literal_s head args =
    let head = Symbol.of_string head in
    let args = List.map
      (function
      | `Var i -> `Var i
      | `Symbol s -> `Symbol (Symbol.of_string s))
      args in
    mk_literal head args

  (** Deconstruct a literal *)
  let open_literal literal =
    let head = literal.(0) in
    let head = i_to_s head in
    let args = Array.to_list (Array.sub literal 1 (Array.length literal - 1)) in
    Symbol.lock ();
    let args = List.map
      (fun i -> if i < 0 then `Var i else `Symbol (i_to_s i))
      args in
    Symbol.unlock ();
    head, args

  (** Create a clause from a conclusion and a list of premises *)
  let mk_clause head premises = Array.of_list (head :: premises)

  (** Deconstruct a clause *)
  let open_clause clause =
    let head = clause.(0) in
    let body = Array.to_list (Array.sub clause 1 (Array.length clause - 1)) in
    head, body

  (** A variable is a negative int *)
  let is_var x = x < 0

  (** Is the literal ground (a fact)? *)
  let is_ground t =
    assert (not (is_var t.(0)));
    let rec check t i =
      if i = Array.length t then true
      else (not (is_var t.(i))) && check t (i+1)
    in
    check t 1

  (** Number of subterms of the literal. Ex for p(a,b,c) it returns 3 *)
  let arity t = Array.length t - 1

  (** compare literals *)
  let compare_literal = Utils.compare_ints

  (** Are the literals equal? *)
  let eq_literal t1 t2 = compare_literal t1 t2 = 0

  (** Hash the literal *)
  let hash_literal t = Utils.hash_ints t

  (** Apply substitution to the literal *)
  let subst_literal subst t =
    if is_ground t || Utils.IHashtbl.length subst = 0
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

  (** Apply substitution to the clause. TODO remove duplicate literals afterward *)
  let subst_clause subst clause =
    if Utils.IHashtbl.length subst = 0 then clause
    else begin
      let a = Array.copy clause in
      for i = 0 to Array.length clause - 1 do
        a.(i) <- subst_literal subst a.(i);
      done;
      a
    end

  (** A datalog clause is safe iff all variables in its head also occur in its body *)
  let check_safe clause =
    let rec check_head i =
      if i = Array.length clause.(0) then true
      else
        let t = clause.(0).(i) in
        if is_var t
          then check_body t 1 && check_head (i+1)
          else check_head (i+1)
    and check_body var j =
      if j = Array.length clause then false
        else check_body_literal var clause.(j) 1 || check_body var (j+1)
    and check_body_literal var literal k =
      if k = Array.length literal then false
      else if literal.(k) = var then true
      else check_body_literal var literal (k+1)
    in
    check_head 1

  (** A fact is a ground clause with empty body *)
  let is_fact clause =
    Array.length clause = 1 && is_ground clause.(0)

  let compare_clause r1 r2 =
    let rec compare r1 r2 i =
      if i = Array.length r1
        then 0
        else
          let cmp = Utils.compare_ints r1.(i) r2.(i) in
          if cmp <> 0 then cmp else compare r1 r2 (i+1)
    in
    if Array.length r1 <> Array.length r2
      then Array.length r1 - Array.length r2
      else compare r1 r2 0

  (** Check whether clauses are (syntactically) equal *)
  let eq_clause r1 r2 = compare_clause r1 r2 = 0

  (** Hash the clause *)
  let hash_clause r =
    let h = ref 17 in
    for i = 0 to Array.length r - 1 do
      h := (!h + 65536) * hash_literal r.(i);
    done;
    abs !h

  (** Remove first body element of the clause, after substitution *)
  let remove_first_subst subst clause =
    assert (Array.length clause > 1);
    let a = Array.make (Array.length clause - 1) [||] in
    a.(0) <- subst_literal subst clause.(0);
    for i = 1 to Array.length clause - 2 do
      a.(i) <- subst_literal subst clause.(i+1);
    done;
    a

  let pp_literal formatter t =
    (* symbol index (int) to string *)
    let to_s s = with_lock (fun () -> Symbol.to_string (i_to_s s)) in
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

  let pp_clause formatter clause =
    if Array.length clause = 1
      then Format.fprintf formatter "%a." pp_literal clause.(0)
      else begin
        Format.fprintf formatter "%a :-@ "  pp_literal clause.(0);
        for i = 1 to Array.length clause - 1 do
          (if i > 1 then Format.fprintf formatter ",@ ");
          Format.fprintf formatter "%a" pp_literal clause.(i);
        done;
        Format.fprintf formatter ".";
      end

  let pp_subst formatter subst =
    let to_s s = with_lock (fun () -> Symbol.to_string (i_to_s s)) in
    Format.fprintf formatter "@[{";
    let first = ref true in
    Utils.IHashtbl.iter
      (fun i j ->
        (if !first then first := false else Format.fprintf formatter ", ");
        Format.fprintf formatter "X%d -> %s@;" (abs i) (to_s j))
      subst;
    Format.fprintf formatter "}@]";

  (* ----------------------------------------------------------------------
   * Generalization/Specialization index on literals
   * ---------------------------------------------------------------------- *)

  (** Hashtable on literals *)
  module LitHashtbl = Hashtbl.Make(
    struct
      type t = literal
      let equal = eq_literal
      let hash = Utils.hash_ints
    end)

  (** Type for an indexing structure on literals *)
  module type Index =
    sig
      type t
        (** A literal index *)

      type elt
        (** A value indexed by a literal *)

      module DataSet : Set.S with type elt = elt
        (** Set of indexed elements *)

      val create : unit -> t
        (** Create a new index *)

      val add : t -> literal -> elt -> unit
        (** Add an element indexed by the literal *)

      val clear : t -> unit
        (** Reset to empty index *)

      val retrieve_generalizations : ('a -> elt -> subst -> 'a) -> 'a -> t -> literal -> 'a
        (** Fold on generalizations of given literal (with transient substitution) *)

      val retrieve_specializations : ('a -> elt -> subst -> 'a) -> 'a -> t -> literal -> 'a
        (** Fold on specifications of given literal (with transient substitution) *)

      val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
        (** Fold on all indexed elements *)

      val is_empty : t -> bool
        (** Is the index empty? *)

      val size : t -> int
        (** Number of indexed elements (linear) *)
    end

  (** Create an Index module for the given type of elements. The implementation
      is based on perfect discrimination trees. *)
  module Make(X : Set.OrderedType) : Index with type elt = X.t =
    struct

      (** A set of indexed data *)
      module DataSet = Set.Make(X)

      (** The literal index. It is a trie with, at each node, a hashset
          of elements, plus a map symbol/var -> subtrie *)
      type t =
      | Node of DataSet.t ref * t Utils.IHashtbl.t

      (** Indexed elements *)
      type elt = X.t

      (** Create a new index *)
      let create () = Node (ref DataSet.empty, Utils.IHashtbl.create 2)

      (** Add the element indexed by the literal *)
      let add t literal elt =
        let len = Array.length literal in
        (* index in subtrie [t], with a cursor at literal[i]. *)
        let rec add t i = match t, i with
        | Node (set, subtries), i when i = len ->
          set := DataSet.add elt !set (* insert in leaf *)
        | Node (_, subtries), i ->
          try
            let subtrie = Utils.IHashtbl.find subtries literal.(i) in
            add subtrie (i+1)
          with Not_found ->
            (* create a new subtrie for the i-th argument of literal, then recurse *)
            let subtrie = Node (ref DataSet.empty, Utils.IHashtbl.create 2) in
            Utils.IHashtbl.add subtries literal.(i) subtrie;
            add subtrie (i+1)
        in
        add t 0

      (** Reset to empty index *)
      let clear t = match t with
        | Node (set, subtries) ->
          set := DataSet.empty;
          Utils.IHashtbl.clear subtries

      (** Fold on generalizations of given ground literal (with transient substitution) *)
      let retrieve_generalizations k acc t literal =
        assert (is_ground literal);
        let subst = Utils.IHashtbl.create 2 in
        let len = Array.length literal in
        (* search in subtrie [t], with cursor at [i]-th argument of [literal] *)
        let rec search t i acc = match t, i with
        | Node (set, _), i when i = len ->
          DataSet.fold (fun elt acc -> k acc elt subst) !set acc
        | Node (_, subtries), i ->
          let sym = literal.(i) in
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

      (** Fold on ground specifications of given literal (with transient substitution) *)
      let retrieve_specializations k acc t literal =
        let subst = Utils.IHashtbl.create 2 in
        let len = Array.length literal in
        (* search in subtrie [t], with cursor at [i]-th argument of [literal] *)
        let rec search t i acc = match t, i with
        | Node (set, _), i when i = len ->
          DataSet.fold (fun elt acc -> k acc elt subst) !set acc
        | Node (_, subtries), i when is_var literal.(i) ->
          let var = literal.(i) in
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
                (* only follow branches leading to ground literals *)
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
            let subtrie = Utils.IHashtbl.find subtries literal.(i) in
            search subtrie (i+1) acc
          with Not_found -> acc
        in
        search t 0 acc

      (** Fold on all indexed elements *)
      let rec fold k acc t = match t with
        | Node (set, subtries) ->
          (* fold on elements at this point *)
          let acc = DataSet.fold (fun elt acc -> k acc elt) !set acc in
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
          DataSet.cardinal !set = 0 && for_all is_empty subtries

      (** Number of elements *)
      let size t = fold (fun i _ -> i + 1) 0 t
    end

  (* ----------------------------------------------------------------------
   * The datalog bipartite resolution algorithm
   * ---------------------------------------------------------------------- *)

  exception UnsafeClause

  module ClausesIndex = Make(
    struct
      type t = clause
      let compare = compare_clause
    end)

  (** Hashtable on clauses *)
  module ClauseHashtbl = Hashtbl.Make(
    struct
      type t = clause
      let equal = eq_clause
      let hash = hash_clause
    end)

  (** Explanation for a clause or fact *)
  type explanation =
    | Axiom
    | Resolution of clause * literal

  (** A database of facts and clauses, with incremental fixpoint computation *)
  type db = {
    db_all : explanation ClauseHashtbl.t;             (** maps all clauses to their explanations *)
    db_facts : ClausesIndex.t;                        (** index on facts *)
    db_clauses : ClausesIndex.t;                      (** index on clauses *)
    db_handlers : (literal -> unit) Utils.IHashtbl.t; (** map symbol -> handler *)
    db_queue : (clause * explanation) Queue.t;        (** queue of clauses to add (+ explanation) *)
  }

  (** Create a DB *)
  let db_create () =
    { db_all = ClauseHashtbl.create 17;
      db_facts = ClausesIndex.create ();
      db_clauses = ClausesIndex.create ();
      db_handlers = Utils.IHashtbl.create 3;
      db_queue = Queue.create ();
    }

  (** Is the clause member of the DB? *)
  let db_mem db clause =
    assert (check_safe clause);
    ClauseHashtbl.mem db.db_all clause

  (** Add the clause/fact to the DB, updating fixpoint *)
  let db_add db clause =
    (if not (check_safe clause) then raise UnsafeClause);
    (* queue of new clauses to insert *)
    let queue = db.db_queue in
    (* is there already a add() going on? *)
    let already_active = not (Queue.is_empty queue) in
    (* add [clause] to the queue of clauses to add *)
    Queue.push (clause, Axiom) queue;
    (* if there is already a add() going on, let it propagate the clause *)
    if already_active then () else
    while not (Queue.is_empty queue) do
      let clause, explanation = Queue.take queue in
      if db_mem db clause then () else begin
      (* clause not already present, add it *)
      ClauseHashtbl.replace db.db_all clause explanation;
      (* generate new clauses by resolution *)
      if is_fact clause
      then begin
        ClausesIndex.add db.db_facts clause.(0) clause;
        (* call handler for this fact, if any *)
        (try let handler = Utils.IHashtbl.find db.db_handlers clause.(0).(0)
             in handler clause.(0)
        with
        | Not_found -> ()
        | e -> (
          Format.eprintf "Datalog: exception while calling handler for %d@." clause.(0).(0);
          raise e));
        (* insertion of a fact: resolution with all clauses whose first body literal
           matches the fact *)
        ClausesIndex.retrieve_generalizations
          (fun () clause' subst ->
            if not (is_fact clause') then
              (* clause' is not a fact, and
                 subst(clause'.body.(0)) = fact, remove the first element of the
                 body of clause', that makes a new clause *)
              let clause'' = remove_first_subst subst clause' in
              let explanation = Resolution (clause', clause.(0)) in
              Queue.push (clause'', explanation) queue)
          () db.db_clauses clause.(0)
      end else begin
        assert (Array.length clause > 1);
        ClausesIndex.add db.db_clauses clause.(1) clause;
        (* insertion of a non_unit clause: resolution with all facts that match the
           first body literal of the clause *)
        ClausesIndex.retrieve_specializations
          (fun () fact subst ->
            (* subst(clause.body.(0)) = fact, remove this first literal *)
            let clause' = remove_first_subst subst clause in
            let explanation = Resolution (clause, fact.(0)) in
            Queue.push (clause', explanation) queue)
          () db.db_facts clause.(1)
      end
      end
    done

  (** match the given literal with facts of the DB, calling the handler on
      each fact that match (with the corresponding substitution) *)
  let db_match db pattern handler =
    ClausesIndex.retrieve_specializations
      (fun () fact subst -> handler fact.(0) subst)
      () db.db_facts pattern

  (** Size of the DB *)
  let db_size db = ClauseHashtbl.length db.db_all

  (** Fold on all clauses in the current DB (including fixpoint) *)
  let db_fold k acc db =
    ClauseHashtbl.fold
      (fun clause _ acc -> k acc clause)
      db.db_all acc

  (** [db_subscribe db symbol handler] causes [handler] to be called with
      any new fact that has head symbol [symbol] from now on *)
  let db_subscribe db symbol handler =
    let i = s_to_i symbol in
    Utils.IHashtbl.replace db.db_handlers i handler

  (** Explain the given fact by returning a list of facts that imply it
      under the current clauses. *)
  let db_explain db fact =
    let module LitSet = Set.Make(struct type t = literal let compare = compare_literal end) in
    let explored = ref ClausesIndex.DataSet.empty
    and set = ref LitSet.empty in
    (* recursively collect explanations *)
    let rec search clause =
      if ClausesIndex.DataSet.mem clause !explored then ()
      else begin
        explored := ClausesIndex.DataSet.add clause !explored;
        let explanation = ClauseHashtbl.find db.db_all clause in
        match explanation with
        | Axiom when is_fact clause -> set := LitSet.add clause.(0) !set
        | Axiom -> ()
        | Resolution (clause, fact) -> begin
          search clause;
          search [|fact|]
        end
      end
    in
    (* once the set is collected, convert it to list *)
    search [|fact|];
    LitSet.elements !set

  (** Immediate premises of the fact (ie the facts that resolved with
      a clause to give the literal), plus the clause that has been used. *)
  let db_premises db fact =
    let rec search acc clause =
      let explanation = ClauseHashtbl.find db.db_all clause in
      match explanation with
      | Axiom -> clause, acc  (* no premises *)
      | Resolution (clause, fact) -> let acc = fact :: acc in search acc clause
    in
    search [] [|fact|]
end

(** Default literal base, where symbols are just strings *)
module Default = Make(
  struct
    type t = string
    let to_string s = s
    let of_string s = s
    let equal s1 s2 = String.compare s1 s2 = 0
    let hash s = Hashtbl.hash s
    let lock () = ()
    let unlock () = ()
  end)
