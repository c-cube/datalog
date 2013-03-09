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

  val empty : t
    (** Empty database *)

  val mem : t -> clause -> bool
    (** Is the clause member of the DB? *)

  val add : t -> clause -> t * result list
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        It returns the list of deduced new results.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val add_fact : t -> literal -> t * result list
    (** Add a fact (ground unit clause) *)

  val add_goal : t -> literal -> t * result list
    (** Add a goal to the DB. The goal is used to trigger backward chaining
        (calling goal handlers that could help solve the goal) *)

  val match_with : t -> literal -> (literal Logic.bind -> subst -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match (with the corresponding substitution) *)

  val db_size : t -> int
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

  val explanations : db -> clause -> explanation Sequence.t
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

  type queue_item =
    [ `AddClause of clause * explanation
    | `AddGoal of literal
    ]

  (** A database of facts and clauses, with incremental fixpoint computation *)
  type t = {
    db_all : explanation ClauseHashtbl.t;   (** maps all clauses to their explanations *)
    db_facts : ClausesIndex.t;              (** index on facts *)
    db_goals : GoalIndex.t;                 (** set of goals *)
    db_selected : ClausesIndex.t;           (** index on clauses' selected premises *)
    db_heads : ClausesIndex.t;              (** index on clauses' heads *)
    db_queue : queue_item Queue.t;          (** queue of items to process *)
  }

  (** Create a DB *)
  let db_create () =
    { db_all = ClauseHashtbl.create 17;
      db_facts = ClausesIndex.create ();
      db_goals = GoalIndex.create ();
      db_selected = ClausesIndex.create ();
      db_heads = ClausesIndex.create ();
      db_fact_handlers = Hashtbl.create 3;
      db_goal_handlers = [];
      db_queue = Queue.create ();
    }

  (** Is the clause member of the DB? *)
  let db_mem db clause =
    assert (check_safe clause);
    ClauseHashtbl.mem db.db_all clause

  let add_clause db clause explanation =
    (* check if clause already present; in any case add the explanation *)
    let already_present = db_mem db clause in
    ClauseHashtbl.add db.db_all clause explanation;
    if already_present then ()
    (* generate new clauses by resolution *)
    else if is_fact clause then begin
      ClausesIndex.add db.db_facts clause.(0) clause;
      (* call handler for this fact, if any *)
      let handlers = Hashtbl.find_all db.db_fact_handlers clause.(0).(0) in
      List.iter (fun h ->
        try h clause.(0)
        with e ->
          Format.eprintf "Datalog: exception while calling handler for %d@."
            clause.(0).(0);
          raise e)
        handlers;
      (* insertion of a fact: resolution with all clauses whose
         first body literal matches the fact. No offset is needed, because
         the fact is ground. *)
      ClausesIndex.retrieve_generalizations
        (fun () _ clause' subst ->
          (* subst(clause'.(1)) = clause.(0) , remove the first element of the
             body of subst(clause'), that makes a new clause *)
          let clause'' = remove_first_subst subst (clause',0) in
          let explanation = Resolution (clause', clause.(0)) in
          Queue.push (`AddClause (clause'', explanation)) db.db_queue)
        () (db.db_selected,0) (clause.(0),0)
    end else begin
      assert (Array.length clause > 1);
      (* check if some goal unifies with head of clause *)
      let offset = offset clause in
      GoalIndex.retrieve_unify
        (fun () goal () subst ->
          (* subst(goal) = subst(clause.(0)), so subst(clause.(1)) is a goal *)
          let new_goal = subst_literal subst (clause.(1),0) in
          Queue.push (`AddGoal new_goal) db.db_queue)
        () (db.db_goals,offset) (clause.(0),0);
      (* add to index *)
      ClausesIndex.add db.db_selected clause.(1) clause;
      ClausesIndex.add db.db_heads clause.(0) clause;
      (* insertion of a non_unit clause: resolution with all facts that match the
         first body literal of the clause *)
      ClausesIndex.retrieve_specializations
        (fun () fact _ subst ->
          (* subst(clause.body.(0)) = fact, remove this first literal *)
          let clause' = remove_first_subst subst (clause,0) in
          let explanation = Resolution (clause, fact) in
          Queue.push (`AddClause (clause', explanation)) db.db_queue)
        () (db.db_facts,offset) (clause.(1),0)
    end

  let add_goal db lit =
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

  (** Push the item in the queue, and process items in the queue
      if no other function call is already doing it *)
  let process_items db item =
    let empty = Queue.is_empty db.db_queue in
    Queue.push item db.db_queue;
    (* how to process one queue item *)
    let process_item item = 
      match item with
      | `AddClause (c, explanation) -> add_clause db c explanation
      | `AddGoal goal -> add_goal db goal
    in
    (* if the queue was not empty, that means that another call
        below in the stack is already processing items. We only
        need to do it if it is not the case *)
    if empty then begin
      while not (Queue.is_empty db.db_queue) do
        let item = Queue.pop db.db_queue in
        process_item item
      done
    end

  (** Add the clause/fact to the DB, updating fixpoint *)
  let db_add db clause =
    (if not (check_safe clause) then raise UnsafeClause);
    process_items db (`AddClause (clause, Axiom))

  (** Add a fact (ground unit clause) *)
  let db_add_fact db lit =
    (if not (is_ground lit) then raise UnsafeClause);
    process_items db (`AddClause ([|lit|], Axiom))

  (** Add a goal to the DB. The goal is used to trigger backward chaining
      (calling goal handlers that could help solve the goal) *)
  let db_goal db lit =
    process_items db (`AddGoal lit)

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

  (** Get all the explanations that explain why this clause is true *)
  let db_explanations db clause =
    ClauseHashtbl.find_all db.db_all clause

  let version = "0.3.1"
end
