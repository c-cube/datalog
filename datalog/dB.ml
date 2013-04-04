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

(** {1 The datalog database} *)

(** {2 Signature of the Database} *)
module type S = sig
  module Logic : Logic.S

  type literal = Logic.literal

  type clause = Logic.clause

  type context = Logic.context

  type t
    (** The type for a database of facts and clauses, with
        incremental fixpoint computation *)

  exception UnsafeClause

  type explanation =
    | Axiom
    | Resolution of clause * literal

  type result =
    | NewFact of literal * explanation
    | NewClause of clause * explanation
    | NewGoal of literal

  type action =
    | AddClause of clause * explanation
    | AddFact of literal * explanation   (* shortcut *)
    | AddGoal of literal

  type handler = result -> action list
    (** A handler is a piece of external code that knows how to interpret
        some results *)

  val empty : unit -> t
    (** Empty database *)

  val copy : t -> t
    (** Get a (shallow) that can be used for backtracking without modifying
        the given DB. This is quite cheap. *)

  val add_handler : t -> handler -> unit
    (** All results of the DB will be given to the handler, from now on.
        Handlers too are copied by [copy]. *)

  val mem : t -> clause -> bool
    (** Is the clause member of the DB? *)

  val propagate : ?on_result:(result -> unit) -> t -> unit
    (** Compute the fixpoint of the current Database's state *)

  (** The modification functions ({!add}, {!add_fact}, {!add_goal}...) modify
      the database, and then update the fixpoint. They return a list of
      {b results}, ie the new information that has been discovered during
      propagation. *)

  val add : ?on_result:(result -> unit) -> t -> clause -> unit
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        It returns the list of deduced new results.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val add_fact : ?on_result:(result -> unit) -> t -> literal -> unit
    (** Add a fact (ground unit clause) *)

  val add_goal : ?on_result:(result -> unit) -> t -> literal -> unit
    (** Add a goal to the DB. The goal is used to trigger backward chaining
        (calling goal handlers that could help solve the goal) *)

  val add_seq : ?on_result:(result -> unit) -> t -> clause Sequence.t -> unit
    (** Add a whole sequence of clauses, in batch. *)

  val add_action : ?on_result:(result -> unit) -> t -> action -> unit
    (** Add an action to perform *)
  
  val add_actions : ?on_result:(result -> unit) -> t -> action Sequence.t -> unit
    (** Add a finite set of actions *)

  val raw_add : t -> action -> unit
    (** Add the action but does not propagate yet. The user needs to call
        {! propagate} by herself *)

  val match_with : t -> context -> literal -> context -> (literal -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match (with its variables bound in its context) *)

  val size : t -> int
    (** Size of the DB *)

  val fold : ('a -> clause -> 'a) -> 'a -> t -> 'a
    (** Fold on all clauses in the current DB (including fixpoint) *)

  val goal_mem : t -> literal -> bool
    (** Is the given literal a goal? *)
    (* XXX: should it be true if the lit is subsumed by a goal? *)

  val goals : t -> literal Sequence.t
    (** Iterate on all current goals *)

  val support : t -> literal -> literal list
    (** Explain the given fact by returning a set of facts that imply it
        under the current clauses, or raise Not_found *)

  val premises : t -> literal -> clause * literal list
    (** Immediate premises of the fact (ie the facts that resolved with
        a clause to give the literal), plus the clause that has been used. *)

  val explanations : t -> clause -> explanation Sequence.t
    (** Get all the explanations that explain why this clause is true *)
end

(** {2 Implementation} *)
module Make(L : Logic.S) = struct
  module Logic = L

  type literal = L.literal

  type clause = L.clause

  type context = L.context

  exception UnsafeClause

  (** Explanation for a clause or fact *)
  type explanation =
    | Axiom
    | Resolution of clause * literal

  type result =
    | NewFact of literal * explanation
    | NewClause of clause * explanation
    | NewGoal of literal

  type action =
    | AddClause of clause * explanation
    | AddFact of literal * explanation   (* shortcut *)
    | AddGoal of literal

  type handler = result -> action list

  type index_data =
    | IdxPremise of clause          (* lit = first premise of clause *)
    | IdxConclusion of clause       (* lit = conclusion of clause *)
    | IdxFact                       (* lit is a fact *)
    | IdxGoal                       (* lit is a goal *)
    (** The kind of data that is put in the global index. It associates
        various data to the literal. *)

  let compare_idx_data data1 data2 =
    match data1, data2 with
    | IdxPremise c1, IdxPremise c2 ->
      L.compare_clause c1 c2
    | IdxConclusion c1, IdxConclusion c2 ->
      L.compare_clause c1 c2
    | IdxFact, IdxFact -> 0
    | IdxGoal, IdxGoal -> 0
    | IdxPremise _, _ -> 1
    | IdxConclusion _, IdxPremise _ -> -1
    | IdxConclusion _, _ -> 1
    | IdxFact, (IdxPremise _ | IdxConclusion _) -> -1
    | IdxFact, _ -> 1
    | IdxGoal, _ -> -1

  module Idx = Logic_terms.DiscrTree(L.T)

  (** The type for a database of facts and clauses, with
      incremental fixpoint computation *)
  type t = {
    mutable db_clauses : explanation list L.ClauseHashtbl.t;   (** Present clauses *)
    mutable db_idx : index_data Idx.t;        (** Global index for lits/clauses *)
    mutable db_handlers : handler list;       (** Handlers *)
    mutable db_queue : action Queue.t;        (** Queue of items to process *)
  }

  (** Empty DB *)
  let empty () =
    { db_clauses = L.ClauseHashtbl.create 5;
      db_idx = Idx.empty ();
      db_handlers = [];
      db_queue = Queue.create ();
    }

  (** Get a (shallow) that can be used for backtracking without modifying
      the given DB. This is quite cheap. *)
  let copy db =
    assert (Queue.is_empty db.db_queue); 
    { db_clauses = db.db_clauses;
      db_idx = db.db_idx;
      db_handlers = db.db_handlers;
      db_queue = Queue.create ();
    }

  (** All results of the DB will be given to the handler, from now on.
      Handlers too are copied by [copy]. *)
  let add_handler db handler =
    db.db_handlers <- handler :: db.db_handlers

  (** Is the clause member of the DB? *)
  let mem db clause =
    L.ClauseHashtbl.mem db.db_clauses clause

  (** Is the given literal a goal? *)
  let goal_mem db goal =
    let ctx1 = L.T.mk_context () in
    let ctx2 = L.T.mk_context () in
    try
      (* check whether the goal is already present *)
      Idx.retrieve_renaming
        (fun () lit _ data ->
          if Gen.exists (function | IdxGoal -> true | _ -> false) data
            then raise Exit)
        () db.db_idx ctx1 goal ctx2;
      false
    with Exit ->
      true

  let add_clause_idx idx clause =
    match clause with
    | L.Clause (head, []) -> assert false
    | L.Clause (head, premise::_) ->
      (* index by [head] *)
      let idx' = Idx.add idx head (IdxConclusion clause) in
      (* index clause by premises in body *)
      Idx.add idx' premise (IdxPremise clause)

  let add_fact_idx idx fact =
    let idx' = Idx.add idx fact IdxFact in
    idx'

  let add_goal_idx idx goal =
    Idx.add idx goal IdxGoal

  (** Forward resolution with the given fact: resolution with clauses in
      which first premise unifies with [fact]. *)
  let fwd_resolution db fact =
    let ctx_idx = L.T.mk_context () in
    let ctx_fact = L.T.mk_context () in
    (* retrieve clauses whose premises unify with fact *)
    Idx.retrieve_unify
      (fun () _ _ data ->
        Gen.iter
          (fun data -> match data with
            | IdxPremise (L.Clause (concl, [_]) as c) ->
              let fact' = L.T.apply concl ctx_idx in
              let explanation = Resolution (c, fact) in
              Queue.push (AddFact (fact', explanation)) db.db_queue
            | IdxPremise c ->
              (* resolution with clause [c] *)
              let c' = L.remove_first c ctx_idx in
              let explanation = Resolution (c, fact) in
              Queue.push (AddClause (c', explanation)) db.db_queue
            | _ -> ())
          data)
      () db.db_idx ctx_idx fact ctx_fact

  (** Backward hyper-resolution of the [clause] with facts present in [db] *)
  let back_resolution db clause =
    match clause with
    | L.Clause (_, []) -> assert false
    | L.Clause (concl, [premise]) ->
      let ctx_clause = L.T.mk_context () in
      let ctx_idx = L.T.mk_context () in
      Idx.retrieve_unify
        (fun () fact _ data ->
          if Gen.exists (function | IdxFact -> true | _ -> false) data
            then (* resolution *)
              let explanation = Resolution (clause, fact) in
              let fact' = L.T.apply concl ctx_clause in
              Queue.push (AddFact (fact', explanation)) db.db_queue)
        () db.db_idx ctx_idx premise ctx_clause
    | L.Clause (_, premise::_) ->
      let ctx_clause = L.T.mk_context () in
      let ctx_idx = L.T.mk_context () in
      Idx.retrieve_unify
        (fun () fact _ data ->
          if Gen.exists (function | IdxFact -> true | _ -> false) data
            then (* resolution *)
              let explanation = Resolution (clause, fact) in
              let clause' = L.remove_first clause ctx_clause in
              Queue.push (AddClause (clause', explanation)) db.db_queue)
        () db.db_idx ctx_idx premise ctx_clause

  let fwd_goal_chaining db goal =
    let ctx_goal = L.T.mk_context () in
    let ctx_idx = L.T.mk_context () in
    (* find clauses that may help solving this goal *)
    Idx.retrieve_unify
      (fun () lit _ data ->
        Gen.iter
          (function
          | IdxConclusion (L.Clause (_, premise::_)) ->
            (* this clause may help solving the goal *)
            let goal' = L.T.apply premise ctx_idx in
            Queue.push (AddGoal goal') db.db_queue
          | _ -> ())
          data)
      () db.db_idx ctx_idx goal ctx_goal

  let back_goal_chaining db clause =
    match clause with
    | L.Clause (concl, premise::_) ->
      let ctx_clause = L.T.mk_context () in
      let ctx_idx = L.T.mk_context () in
      (* find clauses that may help solving this goal *)
      Idx.retrieve_unify
        (fun () goal _ data ->
          Gen.iter
            (function
            | IdxGoal ->
              let goal' = L.T.apply premise ctx_clause in
              Queue.push (AddGoal goal') db.db_queue
            | _ -> ())
            data)
        () db.db_idx ctx_idx concl ctx_clause
    | L.Clause (_, []) -> assert false

  let process_add_clause db ~add_result c explanation =
    try
      (* just add an explanation *)
      let explanations = L.ClauseHashtbl.find db.db_clauses c in
      assert (explanations <> []);
      let explanations = explanation::explanations in
      db.db_clauses <- L.ClauseHashtbl.replace db.db_clauses c explanations
    with Not_found ->
      (* clause is new, update fixpoint and add it to the set of clauses *)
      db.db_clauses <- L.ClauseHashtbl.replace db.db_clauses c [explanation];
      match c with
      (* add the clause to the index, and generate new clauses by resolution *)
      | L.Clause (fact, []) ->
        add_result (NewFact (fact, explanation));
        fwd_resolution db fact;
        (* add fact to the DB *)
        db.db_idx <- add_fact_idx db.db_idx fact;
      | L.Clause (head, _::_) ->
        add_result (NewClause (c, explanation));
        back_resolution db c;
        back_goal_chaining db c;
        (* add clause to the DB *)
        db.db_idx <- add_clause_idx db.db_idx c

  (* TODO goal subsumption; also implement goal chaining *)
  let process_add_goal db ~add_result goal =
    if not (goal_mem db goal) then begin
      add_result (NewGoal goal);
      (* add goal to the index, and chain *)
      db.db_idx <- add_goal_idx db.db_idx goal;
      fwd_goal_chaining db goal;
    end

  (** Process one action *)
  let rec process_action db ~add_result action =
    match action with
    | AddClause (c, explanation) ->
      process_add_clause db ~add_result c explanation
    | AddFact (lit, explanation) ->
      let c = L.mk_clause lit [] in
      process_add_clause db ~add_result c explanation
    | AddGoal goal ->
      process_add_goal db ~add_result goal

  (** Compute the fixpoint of the current Database's state *)
  let propagate ?(on_result=fun _ -> ()) db =
    (* to be called for each new result *)
    let add_result result =
      on_result result;  (* user defined handler *)
      List.iter  (* registered handlers *)
        (fun handler ->
          let actions = handler result in  (* call handler on the result *)
          List.iter (fun action -> Queue.push action db.db_queue) actions)
        db.db_handlers
    in
    while not (Queue.is_empty db.db_queue) do
      let action = Queue.pop db.db_queue in
      process_action db ~add_result action
    done;
    ()

  let add_action ?on_result db action =
    Queue.push action db.db_queue;
    (* compute fixpoint *)
    propagate ?on_result db

  let add_actions ?on_result db actions =
    Sequence.to_queue db.db_queue actions;
    propagate ?on_result db

  (** Add the clause to the Datalog base *)
  let add ?on_result db clause =
    assert (L.check_safe clause);
    add_action ?on_result db (AddClause (clause, Axiom))

  let add_fact ?on_result db fact =
    add ?on_result db (L.mk_clause fact [])

  (** Add the given goal to the Datalog base *)
  let add_goal ?on_result db goal =
    add_action ?on_result db (AddGoal goal)

  let add_seq ?on_result db clauses =
    let open Sequence.Infix in
    (* push batch, then propagate *)
    clauses
      |> Sequence.map (fun c -> AddClause (c, Axiom))
      |> add_actions ?on_result db

  let raw_add db action =
    Queue.push action db.db_queue 

  (* --------------- TODO ------------ *)

  let match_with db ctx_db lit ctx_lit k =
    Idx.retrieve_specializations
      (fun () fact _ data ->
        if Gen.exists (function | IdxFact -> true | _ -> false) data
          then k fact)
      () db.db_idx ctx_db lit ctx_lit

  let size db =
    Idx.fold (fun i _ _ -> i+1) 0 db.db_idx

  let fold f acc db =
    Idx.fold
      (fun acc lit data ->
        Gen.fold
          (fun acc idx_data -> match idx_data with
            | IdxFact -> f acc (L.mk_clause lit [])
            | IdxConclusion c -> f acc c
            | _ -> acc)
        acc data)
      acc db.db_idx

  let goals db =
    Sequence.from_iter
      (fun k ->
        Idx.fold
          (fun () lit data ->
            if Gen.exists (function | IdxGoal -> true | _ -> false) data
              then k lit)
          () db.db_idx)

  let support db lit =
    (*
    let set = L.LitMutHashtbl.create 7 in
    let explored = L.ClauseMutHashtbl.create 7 in
    *)
    [] (* TODO *)

  let premises db lit = failwith "not implemented" (* TODO *)

  let explanations db c = Sequence.empty (* TODO *)

  (*

  (** Explain the given fact by returning a list of facts that imply it
      under the current clauses. *)
  let db_explain db fact =
    let module LitSet = Set.Make(struct type t = literal let compare = compare_literal end) in
    let explored = ref ClausesIndex.DataSet.empty
    and set = ref LitSet.empty in
    (* recursively collect explanations *)
    let rec search clause =
      let elt = clause.(0), clause in
      if ClausesIndex.DataSet.mem elt !explored then ()
      else begin
        explored := ClausesIndex.DataSet.add elt !explored;
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
    search (L.mk_clause fact []);
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

  (** Get all the explanations that explain why this clause is true *)
  let db_explanations db clause =
    ClauseHashtbl.find_all db.db_all clause

  *)
end
