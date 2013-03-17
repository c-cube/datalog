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

  type t
    (** The type for a database of facts and clauses, with
        incremental fixpoint computation *)

  exception UnsafeClause

  type explanation =
    | Axiom
    | Resolution of clause * literal
    (** Explanation for a clause or fact *)

  type result =
    | NewFact of literal
    | NewGoal of literal
    | NewRule of clause

  val empty : unit -> t
    (** Empty database *)

  val copy : t -> t
    (** Get a (shallow) that can be used for backtracking without modifying
        the given DB. This is quite cheap. *)

  val mem : t -> clause -> bool
    (** Is the clause member of the DB? *)

  val propagate : t -> result list
    (** Compute the fixpoint of the current Database's state *)

  val add : t -> clause -> result list
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        It returns the list of deduced new results.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val add_fact : t -> literal -> result list
    (** Add a fact (ground unit clause) *)

  val add_goal : t -> literal -> result list
    (** Add a goal to the DB. The goal is used to trigger backward chaining
        (calling goal handlers that could help solve the goal) *)

  val add_seq : t -> clause Sequence.t -> result list
    (** Add a whole sequence of clauses, in batch. *)

  val match_with : t -> literal -> (literal Logic.bind -> Logic.subst -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match (with the corresponding substitution) *)

  val size : t -> int
    (** Size of the DB *)

  val fold : ('a -> clause -> 'a) -> 'a -> t -> 'a
    (** Fold on all clauses in the current DB (including fixpoint) *)

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

  exception UnsafeClause

  (** Explanation for a clause or fact *)
  type explanation =
    | Axiom
    | Resolution of clause * literal

  type result =
    | NewFact of literal
    | NewGoal of literal
    | NewRule of clause

  type queue_item =
    | AddClause of clause * explanation
    | AddGoal of literal

  type index_data = {
    as_premise : (clause * explanation) list;
    as_conclusion : (clause * explanation) list;
    as_fact : explanation list;
    as_goal : bool;
  } (** The kind of data that is put in the global index. It associates
        various data to the literal. *)

  (** Empty Data associated to a literal *)
  let empty_data = {
    as_premise = [];
    as_conclusion = [];
    as_goal = false;
    as_fact = [];
  }

  module Idx = Index.Make(L)

  (** The type for a database of facts and clauses, with
      incremental fixpoint computation *)
  type t = {
    mutable db_idx : index_data Idx.t;                (** Global index for lits/clauses *)
    db_queue : queue_item Queue.t;                    (** Queue of items to process *)
  }

  (** Empty DB *)
  let empty () =
    { db_idx = Idx.empty ();
      db_queue = Queue.create ();
    }

  (** Get a (shallow) that can be used for backtracking without modifying
      the given DB. This is quite cheap. *)
  let copy db =
    assert (Queue.is_empty db.db_queue); 
    { db_idx = db.db_idx;
      db_queue = Queue.create ();
    }

  (** Is the clause member of the DB? *)
  let mem db clause =
    assert (L.check_safe clause);
    try
      ignore
        (Idx.retrieve_renaming
          (fun () _ _ _ -> raise Exit) ()
          (db.db_idx,0) (L.conclusion clause,1));
      false
    with Exit ->
      true

  let add_fact_idx idx fact explanation =
    Idx.map idx fact
      (function
      | None ->
        let data = empty_data in
        Some {data with as_fact = [explanation]}
      | Some data ->
        Some {data with as_fact = explanation::data.as_fact; })

  let add_clause_idx idx clause explanation =
    match clause with
    | L.Clause (_, []) -> assert false
    | L.Clause (head, lit::_) ->
      (* index clause by lit as premise *)
      let idx' = Idx.map idx lit
        (function
        | None ->
          let data = empty_data in
          Some {data with as_premise = [clause,explanation]; }
        | Some data ->
          Some {data with as_premise = (clause,explanation) :: data.as_premise; }) in
      (* index clause by head as conclusion *)
      Idx.map idx' head
        (function
        | None ->
          let data = empty_data in
          Some {data with as_conclusion = [clause,explanation]; }
        | Some data ->
          Some {data with as_conclusion = (clause,explanation) :: data.as_conclusion; })

  let add_goal_idx idx goal =
    Idx.map idx goal
      (function
        | None ->
          Some {empty_data with as_goal=true; }
        | Some data ->
          Some {data with as_goal=true; })

  (** Forward resolution with the given fact: resolution with clauses in
      which the first premise unifies with fact. *)
  (* TODO *)
  let fwd_resolution db fact =
    (*
    ClausesIndex.retrieve_generalizations
      (fun () _ clause' subst ->
        (* subst(clause'.(1)) = clause.(0) , remove the first element of the
           body of subst(clause'), that makes a new clause *)
        let clause'' = remove_first_subst subst (clause',0) in
        let explanation = Resolution (clause', clause.(0)) in
        Queue.push (`AddClause (clause'', explanation)) db.db_queue)
      () (db.db_selected,0) (clause.(0),0)
    *)
    []

  (** Backward resolution with the clause: resolution with facts that
      unify with the clause's first premise. *)
  (* TODO *)
  let back_resolution db clause =
    (*
    ClausesIndex.retrieve_specializations
      (fun () fact _ subst ->
        (* subst(clause.body.(0)) = fact, remove this first literal *)
        let clause' = remove_first_subst subst (clause,0) in
        let explanation = Resolution (clause, fact) in
        Queue.push (`AddClause (clause', explanation)) db.db_queue)
      () (db.db_facts,offset) (clause.(1),0)
    *)
    []

  (** Forward goal chaining: add this goal to the [db] and chain to find
      other goals *)
  (* TODO *)
  let fwd_goal_chaining db goal =
    (*
    try
      let offset = offset [|lit|] in
      GoalIndex.retrieve_renaming
        (fun () _ _ _ -> raise Exit)
        () (db.db_goals,offset) (lit,0);
      (* goal is not present! call handlers and add it *)
      List.iter (fun h -> h lit) db.db_goal_handlers;
      GoalIndex.add db.db_goals lit ();
      (* find clauses that may help solving this goal *)
      ClausesIndex.retrieve_unify
        (fun () head clause subst ->
          (* subst(clause.(0)) = subst(lit), so subst(clause.(1)) is a new goal *)
          let new_goal = subst_literal subst (clause.(1),offset) in
          Queue.push (`AddGoal new_goal) db.db_queue)
        () (db.db_heads,offset) (lit,0)
    with Exit ->
      (* goal already present (in an alpha-equivalent form) *)
      ()
    *)
    []

  (** Backward goal chaining with the given non unit clause *)
  (* TODO *)
  let back_goal_chaining db clause =
    (*
    let offset = offset clause in
    GoalIndex.retrieve_unify
      (fun () goal () subst ->
        (* subst(goal) = subst(clause.(0)), so subst(clause.(1)) is a goal *)
        let new_goal = subst_literal subst (clause.(1),0) in
        Queue.push (`AddGoal new_goal) db.db_queue)
      () (db.db_goals,offset) (clause.(0),0)
    *)
    []

  let process_add_clause db results c explanation =
    if not (mem db c) then match c with
      (* add the clause to the index, and generate new clauses by resolution *)
      | L.Clause (fact, []) ->
        db.db_idx <- add_fact_idx db.db_idx fact explanation;
        results := fwd_resolution db fact @ !results
      | L.Clause (head, ((_::_) as body)) ->
        db.db_idx <- add_clause_idx db.db_idx c explanation;
        results := back_resolution db c @ !results;
        results := back_goal_chaining db c @ !results

  let process_add_goal db results goal =
    db.db_idx <- add_goal_idx db.db_idx goal;
    results := fwd_goal_chaining db goal @ !results

  (** Process one item *)
  let rec process_queue_item db results item =
    match item with
    | AddClause (c, explanation) -> process_add_clause db results c explanation
    | AddGoal goal -> process_add_goal db results goal

  (** Compute the fixpoint of the current Database's state *)
  let propagate db =
    let results = ref [] in
    while not (Queue.is_empty db.db_queue) do
      let queue_item = Queue.pop db.db_queue in
      process_queue_item db results queue_item
    done;
    !results

  (** Add the clause to the Datalog base *)
  let add db clause =
    assert (L.check_safe clause);
    Queue.push (AddClause (clause, Axiom)) db.db_queue;
    (* fixpoint *)
    propagate db

  let add_fact db fact =
    add db (L.mk_clause fact [])

  (** Add the given goal to the Datalog base *)
  let add_goal db goal =
    Queue.push (AddGoal goal) db.db_queue;
    (* fixpoint *)
    propagate db

  let add_seq db clauses =
    let open Sequence.Infix in
    (* push batch, then propagate *)
    clauses
      |> Sequence.map (fun c -> AddClause (c, Axiom))
      |> Sequence.to_queue db.db_queue;
    propagate db

  (* --------------- TODO ------------ *)

  let match_with db lit k =
    ()  (* TODO *)


  let size db = 0 (* TODO *)

  let fold f acc db = failwith "not implemented" (* TODO *)

  let goals db = Sequence.empty  (* TODO *)

  let support db lit = [] (* TODO *)

  let premises db lit = failwith "not implemented" (* TODO *)

  let explanations db c = Sequence.empty (* TODO *)

  (*

  (** match the given literal with facts of the DB, calling the handler on
      each fact that match (with the corresponding substitution) *)
  let db_match db pattern handler =
    ClausesIndex.retrieve_specializations
      (fun () fact _ subst -> handler (fact,0) subst)
      () (db.db_facts,0) (pattern,1)

  (** Size of the DB *)
  let db_size db =
    ClausesIndex.size db.db_facts + ClausesIndex.size db.db_selected

  (** Fold on all clauses in the current DB (including fixpoint) *)
  let db_fold k acc db =
    ClauseHashtbl.fold
      (fun clause _ acc -> k acc clause)
      db.db_all acc

  let db_subscribe_fact db symbol handler =
    let i = s_to_i symbol in
    Hashtbl.add db.db_fact_handlers i handler

  let db_subscribe_goal db handler =
    db.db_goal_handlers <- handler :: db.db_goal_handlers

  (** Iterate on all current goals *)
  let db_goals db k =
    GoalIndex.fold
      (fun () goal () -> k goal)
      () db.db_goals

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

let version = "0.3.1"  (* TODO move it to Datalog.ml main file *)
