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
    | HyperResolution of clause * literal list
    (** Explanation for a clause or fact *)

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

  val propagate : t -> result Sequence.t
    (** Compute the fixpoint of the current Database's state *)

  (** The modification functions ({!add}, {!add_fact}, {!add_goal}...) modify
      the database, and then update the fixpoint. They return a list of
      {b results}, ie the new information that has been discovered during
      propagation. *)

  val add : t -> clause -> result Sequence.t
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        It returns the list of deduced new results.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val add_fact : t -> literal -> result Sequence.t
    (** Add a fact (ground unit clause) *)

  val add_goal : t -> literal -> result Sequence.t
    (** Add a goal to the DB. The goal is used to trigger backward chaining
        (calling goal handlers that could help solve the goal) *)

  val add_seq : t -> clause Sequence.t -> result Sequence.t
    (** Add a whole sequence of clauses, in batch. *)

  val add_action : t -> action -> result Sequence.t
    (** Add an action to perform *)
  
  val add_actions : t -> action Sequence.t -> result Sequence.t
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
    | HyperResolution of clause * literal list

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
    | IdxPremise of clause * int    (* lit = i-th premise of clause *)
    | IdxConclusion of clause       (* lit = conclusion of clause *)
    | IdxFact                       (* lit is a fact *)
    | IdxGoal                       (* lit is a goal *)
    (** The kind of data that is put in the global index. It associates
        various data to the literal. *)

  let compare_idx_data data1 data2 =
    match data1, data2 with
    | IdxPremise (c1,i1), IdxPremise (c2,i2) ->
      if i1 <> i2 then i1 - i2 else L.compare_clause c1 c2
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
    | L.Clause (head, body) ->
      (* index by [head] *)
      let idx' = Idx.add idx head (IdxConclusion clause) in
      (* index clause by premises in body *)
      let idx', _ =
        List.fold_left
          (fun (idx,i) premise ->
            Idx.add idx premise (IdxPremise (clause, i)), i+1)
          (idx', 0) body in
      idx'

  let add_fact_idx idx fact =
    let idx' = Idx.add idx fact IdxFact in
    idx'

  let add_goal_idx idx goal =
    Idx.add idx goal IdxGoal

  (** Given the list of premises, and the conclusion, perform hyper-resolution
      with all facts present in [idx]. [ctx] is the current context for
      the variables in [premises] and [conclusions]. [send_fact] is called
      with all deduced facts and their premises. *)
  let hyperresolve ~send_fact conclusion premises ctx idx =
    (* heuristic: sort premises by decreasing size; bigger terms have more
       chances to have few matches *)
    let premises = List.sort (fun t1 t2 -> t2.L.T.size - t1.L.T.size) premises in
    let premises = List.map (fun t -> t, L.T.mk_context ()) premises in
    (* unify each premise with facts; [facts] is the list of facts used
       so far to resolve previous premises *)
    let rec resolve facts premises = match premises with
      | [] ->
        (* hyperresolution succeeded! *)
        let fact = L.T.apply conclusion ctx in
        send_fact fact facts
      | (p, ctx_idx)::premises' ->
        (* for each fact that match [p], recurse *)
        Idx.retrieve_unify
          (fun () fact _ data ->
            Gen.iter
              (fun data -> match data with
               | IdxFact -> resolve (fact::facts) premises'
               | _ -> ())
              data)
          () idx ctx_idx p ctx 
    in
    resolve [] premises

  (** Remove [n]-th element of [l] *)
  let rec remove_list_nth l n = match l with
    | [] -> failwith "remove_list_nth: index not in list"
    | x::l' when n=0 -> l'
    | x::l' -> x::(remove_list_nth l' (n-1))

  (** Forward hyper-resolution with the given fact: resolution with clauses in
      which some premise unifies with [fact]. *)
  let fwd_resolution db fact =
    let ctx_idx = L.T.mk_context () in
    let ctx_fact = L.T.mk_context () in
    let results = Queue.create () in
    (* retrieve clauses whose premises unify with fact *)
    Idx.retrieve_unify
      (fun () _ _ data ->
        Gen.iter
          (fun data -> match data with
            | IdxPremise ((L.Clause (concl, body)) as clause, i) ->
              (* hyperresolution with this clause *)
              let send_fact f facts =
                let explanation = HyperResolution (clause, fact::facts) in
                Queue.push (NewFact (f, explanation)) results
              in
              let premises = remove_list_nth body i in
              hyperresolve ~send_fact concl premises ctx_idx db.db_idx
            | _ -> ())
          data)
      () db.db_idx ctx_idx fact ctx_fact;
    Sequence.of_queue results

  (** Backward hyper-resolution of the [clause] with facts present in [db] *)
  let back_resolution db clause =
    match clause with
    | L.Clause (_, []) ->
      Sequence.empty  (* no resolution, this is a fact *)
    | L.Clause (concl, premises) ->
      let ctx = L.T.mk_context () in
      let results = Queue.create () in
      let send_fact f facts =
        let explanation = HyperResolution (clause, facts) in
        Queue.push (NewFact (f, explanation)) results
      in
      hyperresolve ~send_fact concl premises ctx db.db_idx;
      Sequence.of_queue results

  let fwd_goal_chaining db goal =
    Sequence.empty  (* TODO *)

  let back_goal_chaining db clause =
    Sequence.empty  (* TODO *)

  (*

  (** Forward goal chaining: add this goal to the [db] and chain to find
      other goals *)
  let fwd_goal_chaining db goal =
    let offset = L.lit_offset goal in
    (* find clauses that may help solving this goal *)
    Idx.retrieve_unify
      (fun acc lit data subst ->
        (* find clauses whose head unified with goal *)
        List.fold_left
          (fun acc (clause, _) ->
            match clause with
            | L.Clause (_, []) -> acc
            | L.Clause (_, lit'::_) ->
              let new_goal = L.subst_literal subst (lit',offset) in
              AddGoal new_goal :: acc)
          acc data.as_conclusion)
      [] (db.db_idx,offset) (goal,0)

  (** Backward goal chaining with the given non unit clause *)
  let back_goal_chaining db clause =
    match clause with
    | L.Clause (_, []) -> []
    | L.Clause (head, lit::_) ->
      let offset = L.offset clause in
      Idx.retrieve_unify
        (fun acc goal data subst ->
          let new_goal = L.subst_literal subst (lit,0) in
          AddGoal new_goal :: acc)
        [] (db.db_idx,offset) (head,0)

  *)

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
        Sequence.iter (fun r -> add_result r) (fwd_resolution db fact);
        (* add fact to the DB *)
        db.db_idx <- add_fact_idx db.db_idx fact;
      | L.Clause (head, _::_) ->
        add_result (NewClause (c, explanation));
        Sequence.iter add_result (back_resolution db c);
        Sequence.iter add_result (back_goal_chaining db c);
        (* add clause to the DB *)
        db.db_idx <- add_clause_idx db.db_idx c

  (* TODO goal subsumption; also implement goal chaining *)
  let process_add_goal db ~add_result goal =
    if not (goal_mem db goal) then begin
      add_result (NewGoal goal);
      (* add goal to the index, and chain *)
      db.db_idx <- add_goal_idx db.db_idx goal;
      Sequence.iter add_result (fwd_goal_chaining db goal);
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
  let propagate db =
    let results = Queue.create () in
    (* to be called for each new result *)
    let add_result result =
      Queue.push result results;  (* save result *)
      (* re-inject the result into the DB *)
      (match result with
      | NewFact (f, explanation) -> Queue.push (AddFact (f, explanation)) db.db_queue
      | NewClause (c, explanation) -> Queue.push (AddClause (c, explanation)) db.db_queue
      | NewGoal goal -> Queue.push (AddGoal goal) db.db_queue);
      List.iter
        (fun handler ->
          let actions = handler result in  (* call handler on the result *)
          List.iter (fun action -> Queue.push action db.db_queue) actions)
        db.db_handlers
    in
    while not (Queue.is_empty db.db_queue) do
      let action = Queue.pop db.db_queue in
      process_action db ~add_result action
    done;
    Sequence.of_queue results

  let add_action db action =
    Queue.push action db.db_queue;
    (* compute fixpoint *)
    propagate db

  let add_actions db actions =
    Sequence.to_queue db.db_queue actions;
    propagate db

  (** Add the clause to the Datalog base *)
  let add db clause =
    assert (L.check_safe clause);
    add_action db (AddClause (clause, Axiom))

  let add_fact db fact =
    add db (L.mk_clause fact [])

  (** Add the given goal to the Datalog base *)
  let add_goal db goal =
    add_action db (AddGoal goal)

  let add_seq db clauses =
    let open Sequence.Infix in
    (* push batch, then propagate *)
    clauses
      |> Sequence.map (fun c -> AddClause (c, Axiom))
      |> add_actions db

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
