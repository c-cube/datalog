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

(** {6 Top-Down Computation} *)

(** This module implements top-down computation of Datalog queries
    with non-stratified negation.

    See "efficient top-down computation of queries under the well-founded
    semantics"
*)

exception NonStratifiedProgram

(** {2 Signature for DB} *)

module type S = sig
  module Base : Base.S
  type const = Base.Const.t
  type scope = Base.scope

  (** A DB stores facts and clauses, that constitute a logic program.
      Facts and clauses can only be added.

      Non-stratified programs will be rejected with NonStratifiedProgram.  *)

  type t
    (** A database is a repository for Datalog clauses. *)

  type interpreter = Base.T.t -> Base.C.t list
    (** Interpreted predicate. It takes terms which have a given
        symbol as head, and return a list of (safe) clauses that
        have the same symbol as head, and should unify with the
        query term. *)

  val create : ?parent:t -> unit -> t

  val copy : t -> t

  val add_fact : t -> Base.T.t -> t
  val add_facts : t -> Base.T.t list -> t

  val add_clause : t -> Base.C.t -> t
  val add_clauses : t -> Base.C.t list -> t

  val interpret : ?help:string -> t -> const -> interpreter -> t
    (** Add an interpreter for the given constant. Goals that start with
        this constant will be given to all registered interpreters, all
        of which can add new clauses. The returned clauses must
        have the constant as head symbol. *)

  val interpret_list : t -> (const * string * interpreter) list -> t
    (** Add several interpreters, with their documentation *)

  val is_interpreted : t -> const -> bool
    (** Is the constant interpreted by some OCaml code? *)

  val add_builtin : t -> Base.Const.t -> Base.BuiltinFun.t -> t
    (** Add a builtin fun *)

  val builtin_funs : t -> Base.BuiltinFun.map

  val eval : t -> Base.T.t -> Base.T.t
    (** Evaluate the given term at root *)

  val help : t -> string list
    (** Help messages for interpreted predicates *)

  val num_facts : t -> int
  val num_clauses : t -> int
  val size : t -> int

  val find_facts : ?oc:bool -> t -> scope -> Base.T.t -> scope ->
                   (Base.T.t -> Base.Subst.t -> unit) -> unit
    (** find facts unifying with the given term, and give them
        along with the unifier, to the callback *)

  val find_clauses_head : ?oc:bool -> t -> scope -> Base.T.t -> scope ->
                          (Base.C.t -> Base.Subst.t -> unit) -> unit
    (** find clauses whose head unifies with the given term,
        and give them along with the unifier, to the callback *)

  val find_interpretation : ?oc:bool -> t -> scope -> Base.T.t -> scope ->
                            (Base.C.t -> Base.Subst.t -> unit) -> unit
    (** Given an interpreted goal, try all interpreters on it,
        and match the query against their heads. Returns clauses
        whose head unifies with the goal, along with the substitution. *)

  (** {2 Query} *)

  val ask : ?oc:bool -> ?with_rules:Base.C.t list -> ?with_facts:Base.T.t list ->
            t -> Base.T.t -> Base.T.t list
    (** Returns the answers to a query in a given DB.
        Additional facts and rules can be added in a local scope.
        @param oc enable occur-check in unification (default [false]) *)

  val ask_lits : ?oc:bool -> ?with_rules:Base.C.t list -> ?with_facts:Base.T.t list ->
                 t -> Base.T.t list -> Base.Lit.t list -> Base.T.t list
    (** Extension of {! ask}, where the query ranges over the list of
        variables (the term list), all of which must be bound in
        the list of literals that form a constraint.

        [ask_lits db vars lits] queries over variables [vars] with
        the constraints given by [lits]. 

        Conceptually, the query adds a clause (v1, ..., vn) :- lits, which
        should respect the same safety constraint as other clauses.

        @return a list of answers, each of which is a list of terms that
          map to the given list of variables. *)
end

module Make(Base : Base.S) = struct
  module Base = Base
  module T = Base.T
  module C = Base.C
  module Subst = Base.Subst
  type const = Base.Const.t
  type scope = Subst.scope

  let _debug_enabled = ref false
  let _debug format = 
    if !_debug_enabled
      then
        Printf.kfprintf
          (fun oc -> output_char oc '\n')
          stderr format
      else
        Printf.ifprintf stderr format

  let set_debug b = _debug_enabled := b

  (* TODO: aggregate {b functions} that collapse all their arguments
            into a constant (e.g., sum, average, max, min).
            Plug this into [slg_complete_aggregate]. *)

  (* TODO: dependency graph to check whether program is stratified *)

  (* TODO: reification of DB, with open(db) predicate that evaluates the rest
          of the clause in the scope of the given DB (parent: current context)*)

  (* TODO: on-disk DB, for instance append-only set of Bencode records?
          see the dict format google uses for bigtable *)

  type interpreter = T.t -> C.t list
    (** Interpreted predicate *)

  module ClauseIndex = Base.Index(struct
    type t = C.t
    let equal = Base.Unif.clause_are_alpha_equiv
    let hash = C.hash_novar
  end)
  module TermIndex = Base.Index(struct
    type t = T.t
    let equal = Base.Unif.are_alpha_equiv
    let hash = T.hash_novar
  end)

  module ConstTbl = PersistentHashtbl.Make(Base.Const)

  type t = {
    rules : ClauseIndex.t;  (* clauses with non null body *)
    facts : TermIndex.t;  (* set of facts *)
    interpreters : interpreter list ConstTbl.t; 
      (* constants -> interpreters *)
    builtin : Base.BuiltinFun.map;
    help : string list;
    parent : t option;  (* for further query *)
  }
  type db = t

  let create ?parent () =
    let db = {
      rules = ClauseIndex.empty ();
      facts = TermIndex.empty ();
      interpreters = ConstTbl.create 7;
      builtin = Base.BuiltinFun.create ();
      help = [];
      parent;
    } in
    db

  let rec copy db =
    let rules = ClauseIndex.copy db.rules in
    let facts = TermIndex.copy db.facts in
    let interpreters = ConstTbl.copy db.interpreters in
    let parent = match db.parent with
      | None -> None
      | Some db' -> Some (copy db')
    in
    { db with rules; facts; parent; interpreters; }

  let add_fact db t =
    { db with facts =TermIndex.add db.facts t t; }

  let add_facts db l = List.fold_left add_fact db l

  let add_clause db c =
    match c.C.body with
    | [] -> add_fact db c.C.head
    | _::_ -> {db with rules = ClauseIndex.add db.rules c.C.head c; }

  let add_clauses db l = List.fold_left add_clause db l

  let builtin_funs db = db.builtin

  let add_builtin db c f =
    {db with builtin=Base.BuiltinFun.add db.builtin c f}

  let rec eval db t =
    let t' = Base.BuiltinFun.eval db.builtin t in
    if t == t'
      (* try with parent DB, may have more success *)
      then match db.parent with
        | None -> t'
        | Some db' -> eval db' t'
      else eval db t'

  let interpret ?help db c inter =
    let help = match help with
    | None -> Printf.sprintf "<symbol %s>" (Base.Const.to_string c)
    | Some h -> h
    in
    let db = { db with help=help::db.help; } in
    try
      let l = ConstTbl.find db.interpreters c in
      {db with interpreters=ConstTbl.replace db.interpreters c (inter :: l)}
    with Not_found ->
      {db with interpreters=ConstTbl.replace db.interpreters c [inter]}

  let interpret_list db l =
    List.fold_left (fun db (c, help, i) -> interpret ~help db c i) db l

  let is_interpreted db c =
    ConstTbl.mem db.interpreters c

  let help db =
    let rec help acc db =
      let acc = List.rev_append db.help acc in
      match db.parent with
      | None -> acc
      | Some db' -> help acc db'
    in help [] db

  let num_facts db = TermIndex.size db.facts

  let num_clauses db = ClauseIndex.size db.rules

  let size db = num_facts db + num_clauses db
  
  let rec find_facts ?(oc=false) db s_db t s_t k =
    TermIndex.unify ~oc db.facts s_db t s_t k;
    match db.parent with
    | None -> ()
    | Some db' -> find_facts ~oc db' s_db t s_t k
  
  let rec find_clauses_head ?(oc=false) db s_db t s_t k =
    ClauseIndex.unify ~oc db.rules s_db t s_t k;
    match db.parent with
    | None -> ()
    | Some db' -> find_clauses_head ~oc db' s_db t s_t k

  let rec find_interpretation ?(oc=false) db s_db t s_t k =
    assert (not (T.is_var t));
    begin try
      let c = match T.head_symbol t with
        | None -> raise Not_found
        | Some c -> c
      in
      let interpreters = ConstTbl.find db.interpreters c in
      List.iter
        (fun inter ->
          (* call interpreter to get clauses of its extension *)
          let clauses = inter t in
          List.iter
            (fun clause ->
              try
                let subst = Base.Unif.unify ~oc t s_t clause.C.head s_db in
                k clause subst   (* clause unifies! *)
              with Base.Unif.Fail -> ())
            clauses)
        interpreters
    with Not_found -> ()
    end;
    match db.parent with
    | None -> ()
    | Some db' -> find_interpretation ~oc db' s_db t s_t k

  (** {2 Query} *)

  module Query = struct
    type t = {
      db : db;
      oc : bool;                              (* perform occur-check? *)
      forest : goal_entry Base.TVariantTbl.t; (* forest of goals *)
      renaming : Subst.renaming;              (* renaming *)
      mutable stack : action;                 (* stack of actions to do *)
    } (** A global state for querying *)

    and action =
      | Done
      | Enter of goal_entry * action
      | NewClause of goal_entry * C.t * action
      | Aggregate of goal_entry * C.t * T.t * action
      | Exit of goal_entry * action

    and goal_entry = {
      goal : T.t;
      mutable answers : unit T.Tbl.t;         (* set of answers *)
      mutable poss : (goal_entry * C.t) list; (* positive waiters *)
      mutable negs : (goal_entry * C.t) list; (* negative waiters *)
      mutable complete : bool;                (* goal evaluation completed? *)
    } (** Root of the proof forest *)

    (** In a goal entry, [poss] and [negs] are other goals that depend
        on this given goal. IT's the reverse dependency graph.
        When an answer is added to this goal, it's also propagated
        to waiters. *)

    let create ~oc ~db =
      let query = {
        db;
        oc;
        forest = Base.TVariantTbl.create 127;
        stack = Done;
        renaming = Subst.create_renaming ();
      } in
      query

    (* reset and return the renaming *)
    let _get_renaming ~query =
      Subst.reset_renaming query.renaming;
      query.renaming

    (* try to resolve fact with clause's first body literal *)
    let resolve ~query fact clause =
      match clause.C.body with
      | (Base.Lit.LitPos lit) :: body' ->
        begin try
          let subst = Base.Unif.unify ~oc:query.oc fact 0 lit 1 in
          let renaming = _get_renaming ~query in
          Some (C.mk_clause
            (Subst.eval subst ~renaming clause.C.head 1)
            (Subst.eval_lits subst ~renaming body' 1)
          )
        with Base.Unif.Fail -> None
        end
      | _ -> None

    let _iter_answers k node =
      T.Tbl.iter (fun t () -> k t) node.answers

    let _get_aggr c = match c.C.body with
      | Base.Lit.LitAggr a :: _ -> a
      | _ -> assert false

    (* main loop for the DFS traversal of SLG resolution *)
    let rec slg_main ~query =
      match query.stack with
      | Done -> ()  (* done *)
      | Enter (goal_entry, stack') ->
        query.stack <- stack';
        (* expand goal entry *)
        slg_subgoal ~query goal_entry;
        slg_main ~query
      | Exit (goal_entry, stack') ->
        query.stack <- stack';
        (* close goal entry *)
        if not goal_entry.complete
          then slg_complete ~query goal_entry;
        slg_main ~query
      | NewClause (goal_entry, clause, stack') ->
        query.stack <- stack';
        (* process new clause in the forest of [goal_entry] *)
        slg_newclause ~query goal_entry clause;
        slg_main ~query
      | Aggregate (goal_entry, clause, subgoal, stack') ->
        query.stack <- stack';
        (* compute the answer of the aggregate *)
        let subgoal_entry = Base.TVariantTbl.find query.forest subgoal in
        slg_complete_aggregate ~query goal_entry clause subgoal_entry.answers;
        slg_main ~query

    (* solve the [goal] by all possible means. Returns the goal_entry
       for this goal. *)
    and slg_solve ~query goal =
      _debug "slg_solve with %a" T.pp goal;
      try
        Base.TVariantTbl.find query.forest goal
      with Not_found ->
        (* new goal! insert it in the forest, and start solving it *)
        let goal_entry = {
          goal;
          answers = T.Tbl.create 7;
          poss = [];
          negs = [];
          complete = false;
        } in
        let query = {query with
          forest=Base.TVariantTbl.replace query.forest goal goal_entry}
        in
        (* push the goal on stack so that it is solved *)
        query.stack <- Enter (goal_entry, Exit (goal_entry, query.stack));
        goal_entry

    (* [goal_entry] is a fresh goal, resolve it with facts and clauses to
       obtain its answers *)
    and slg_subgoal ~query goal_entry =
      _debug "slg_subgoal with %a" T.pp goal_entry.goal;
      find_facts ~oc:query.oc query.db 1 goal_entry.goal 0
        (fun fact subst ->
          let renaming = _get_renaming ~query in
          let answer = Subst.eval subst ~renaming goal_entry.goal 0 in
          (* process the new answer to the goal *)
          slg_answer ~query goal_entry answer);
      (* resolve with rules *)
      find_clauses_head ~oc:query.oc query.db 1 goal_entry.goal 0
        (fun clause subst ->
          let renaming = _get_renaming ~query in
          let clause' = Subst.eval_clause subst ~renaming clause 1 in
          (* add a new clause to the forest of [goal] *)
          query.stack <- NewClause (goal_entry, clause', query.stack));
      (* resolve with interpreters *)
      find_interpretation ~oc:query.oc query.db 1 goal_entry.goal 0
        (fun clause subst ->
          let renaming = _get_renaming ~query in
          let clause' = Subst.eval_clause subst ~renaming clause 1 in
          (* add a new clause to the forest of [goal] *)
          query.stack <- NewClause (goal_entry, clause', query.stack));
      ()

    (* called when a new clause appears in the forest of [goal] *)
    and slg_newclause ~query goal_entry clause =
      _debug "slg_newclause with %a and clause %a" T.pp goal_entry.goal C.pp clause;
      match clause.C.body with
      | [] ->
        (* new fact (or clause with only delayed lits) *)
        slg_answer ~query goal_entry clause.C.head
      | (Base.Lit.LitPos subgoal)::_ ->
        (* positive subgoal  *)
        slg_positive ~query goal_entry clause subgoal
      | (Base.Lit.LitNeg neg_subgoal)::body' when T.ground neg_subgoal ->
        (* negative subgoal: if neg_subgoal is solved, continue with clause' *)
        let clause' = C.set_body clause body' in
        slg_negative ~query goal_entry clause' neg_subgoal
      | (Base.Lit.LitAggr a)::_ ->
        (* aggregate: subgoal is a.guard *)
        slg_aggregate ~query goal_entry clause a.Base.Lit.guard
      | _ -> failwith "slg_newclause with non-ground negative goal"

    (* add an answer [ans] to the given [goal]. If [ans] is new,
      insert it into the list of answers of [goal], and update
      positive and negative dependencies *)
    and slg_answer ~query goal_entry ans =
      _debug "slg_answer: %a" T.pp ans;
      assert (T.ground ans);
      if not goal_entry.complete
      && not (T.Tbl.mem goal_entry.answers ans) then begin
        (* new answer! *)
        T.Tbl.add goal_entry.answers ans ();
        (* it's a fact, negative dependencies must fail *)
        goal_entry.negs <- [];
        (* resolve ans.head with positive dependencies *)
        List.iter
          (fun (goal', clause') ->
            match resolve ~query ans clause' with
            | None -> ()
            | Some clause'' ->
              (* resolution succeeded, add clause to the forest of [goal'] *)
              query.stack <- NewClause (goal', clause'', query.stack))
          goal_entry.poss;
      end

    (* positive subgoal. *)
    and slg_positive ~query goal_entry clause subgoal =
      _debug "slg_positive %a with clause %a, subgoal %a"
        T.pp goal_entry.goal C.pp clause T.pp subgoal;
      let subgoal_entry = slg_solve ~query subgoal in
      (* register for future answers *)
      subgoal_entry.poss <- (goal_entry, clause) :: subgoal_entry.poss;
      (* use current answers *)
      T.Tbl.iter
        (fun ans () -> match resolve ~query ans clause with
          | None -> ()
          | Some clause' ->
            query.stack <- NewClause(goal_entry, clause', query.stack))
        subgoal_entry.answers;
      ()

    (* negative subgoal *)
    and slg_negative ~query goal_entry clause neg_subgoal =
      _debug "slg_negative %a with clause %a, neg_subgoal %a"
        T.pp goal_entry.goal C.pp clause T.pp neg_subgoal;
      let subgoal_entry = slg_solve ~query neg_subgoal in
      if T.Tbl.length subgoal_entry.answers = 0
        then if subgoal_entry.complete
          then
            (* success, the negative goal has been solved *)
            slg_newclause ~query goal_entry clause
          else
            (* negative subgoal can still succeed, wait for completion *)
            subgoal_entry.negs <- (goal_entry, clause) :: subgoal_entry.negs
        else
          () (* failure, there are already positive answers; do nothing *)

    (* subgoal is the guard of the agregate in [clause] *)
    and slg_aggregate ~query goal_entry clause subgoal =
      _debug "slg_aggregate %a with clause %a, subgoal %a"
        T.pp goal_entry.goal C.pp clause T.pp subgoal;
      (* before querying subgoal, prepare to gather its results *)
      query.stack <- Aggregate(goal_entry, clause, subgoal, query.stack);
      (* start subquery, and wait for it to complete *)
      let _ = slg_solve ~query subgoal in
      ()

    (* called exactly once, when the subgoal has completed *)
    and slg_complete_aggregate ~query goal_entry clause answers =
      _debug "slg_complete_aggregate %a with %a (%d ans)"
        T.pp goal_entry.goal C.pp clause (T.Tbl.length answers);
      let a = _get_aggr clause in
      (* gather all answers *)
      let renaming = _get_renaming ~query in
      let l = T.Tbl.fold
        (fun t () acc ->
          try
            (* unify a.guard with the answer, and extract the binding of a.var *)
            let subst = Base.Unif.unify a.Base.Lit.guard 0 t 1 in
            let t' = Subst.eval subst ~renaming a.Base.Lit.var 0 in
            t' :: acc
          with Base.Unif.Fail -> failwith "could not unify with var?!")
        answers []
      in
      let right = T.mk_apply_l a.Base.Lit.constructor l in
      (* now unify left with right *)
      try
        let subst = Base.Unif.unify ~oc:query.oc a.Base.Lit.left 0 right 1 in
        let renaming = _get_renaming ~query in
        let answer = Subst.eval subst ~renaming a.Base.Lit.left 0 in
        (* eval *)
        let answer = eval query.db answer in
        (* add answer *)
        slg_answer ~query goal_entry answer
      with Base.Unif.Fail ->
        (* answer aggregate does not match left *)
        ()

    (* goal is completely evaluated, no more answers will arrive. *)
    and slg_complete ~query goal_entry =
      _debug "slg_complete %a" T.pp goal_entry.goal;
      assert (not goal_entry.complete);
      goal_entry.complete <- true;
      if T.Tbl.length goal_entry.answers = 0
        then begin
          (* all negative goals succeed *)
          List.iter
            (fun (goal, clause) -> slg_newclause ~query goal clause)
            goal_entry.negs
        end;
      (* reclaim memory *)
      goal_entry.negs <- [];
      goal_entry.poss <- [];
      ()
  end

  let ask ?(oc=false) ?(with_rules=[]) ?(with_facts=[]) db lit =
    (* create a DB on top of the given one? *)
    let db = match with_rules, with_facts with
    | [], [] -> db
    | _ ->
      let db' = create ~parent:db () in
      let db' = add_facts db' with_facts in
      let db' = add_clauses db' with_rules in
      db'
    in
    let query = Query.create ~oc ~db in
    (* recursive search for answers *)
    let goal_node = Query.slg_solve ~query lit in
    Query.slg_main ~query;
    (* get fact answers *)
    let l = ref [] in
    Query._iter_answers (fun ans -> l := ans :: !l) goal_node;
    !l

  let ask_lits ?(oc=false) ?(with_rules=[]) ?(with_facts=[]) db vars lits =
    (* special clause that defines the query *)
    let head = T.mk_apply Base.Const.query (Array.of_list vars) in
    let clause = C.mk_clause head lits in
    let with_rules = clause :: with_rules in
    let l = ask ~oc ~with_rules ~with_facts db head in
    l
end

(** {2 Default Implementation with Strings} *)


module Default = struct
  module B = Base
  include Make(Base.Default)

  let default_interpreters =
    let _less goal =
      _debug "call less with %a" Base.T.pp goal;
      match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a < b -> [ C.mk_fact goal ]
      | _ -> []
    and _lesseq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a <= b -> [ C.mk_fact goal ]
      | _ -> []
    and _greater goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a > b -> [ C.mk_fact goal ]
      | _ -> []
    and _greatereq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a >= b -> [ C.mk_fact goal ]
      | _ -> []
    and _eq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a = b -> [ C.mk_fact goal ]
      | _ -> []
    and _neq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a <> b -> [ C.mk_fact goal ]
      | _ -> []
    and _print goal =
      begin match goal with
      | T.Apply (_, [| a |]) when T.ground a ->
        Printf.printf "> %a\n" T.pp a;
      | _ -> ()
      end;
      [ C.mk_fact goal ]
    (* given a list of arguments, "replace" the goal by any of its arguments.
       this allow arguments (variables...) to get to the proposition level *)
    and _eval goal = match goal with
      | T.Apply (B.String "eval", subgoals) ->
        (* for each goal \in subgoals, add a clause  goal :- subgoal *)
        Array.fold_left
          (fun acc sub -> C.mk_clause goal [Base.Lit.mk_pos sub] :: acc)
          [] subgoals
      | _ -> []
    in
    [ B.String "lt", "lt(a,b): true if a < b", _less
    ; B.String "<", "a < b", _less
    ; B.String "le", "leq(a,b): true if a <= b", _lesseq
    ; B.String "<=", "a <= b", _lesseq
    ; B.String "gt", "gt(a,b): true if a > b", _greater
    ; B.String ">", "a > b", _greater
    ; B.String "ge", "geq(a, b): true if a >= b", _greatereq
    ; B.String ">=", "a >= b", _greatereq
    ; B.String "eq", "eq(a,b): true if a = b", _eq
    ; B.String "=", "=", _eq
    ; B.String "neq", "neq(a, b): true if a != b", _neq
    ; B.String "!=", "!=", _neq
    ; B.String "print", "print(a): print a term on stdout", _print
    ; B.String "eval", "eval(*goals): add eval(goals) :- g for each g in goals", _eval
    ]

  let _sum t = match t with
    | T.Apply (_, arr) ->
      begin try
        let x = Array.fold_left
          (fun x t' -> match t' with
            | T.Apply (B.Int i, [| |]) -> i+x
            | _ -> raise Exit)
          0 arr
        in
        Some (T.mk_const (B.Int x))
      with Exit -> None
      end
    | _ -> None

  let builtin =
    [ B.String "sum", _sum
    ]

  let setup_default db =
    let db = interpret_list db default_interpreters in
    let db = {db with
      builtin=Base.BuiltinFun.add_list (builtin_funs db) builtin;
    } in
    db
end
