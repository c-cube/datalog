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

(** {1 Main Datalog module} *)

(** Module type for logic *)
module type S = sig
  module Base : Base.S
  type const = Base.Const.t

  (** {2 Higher level API} *)

  (** This part of the API can be used to avoid building variables
      yourself. Calling [quantify3 f] with call [f] with 3 distinct
      variables, and [f] can use those variables to, for instance,
      build a clause *)

  val quantify1 : (Base.T.t -> 'a) -> 'a
  val quantify2 : (Base.T.t -> Base.T.t -> 'a) -> 'a
  val quantify3 : (Base.T.t -> Base.T.t -> Base.T.t -> 'a) -> 'a
  val quantify4 : (Base.T.t -> Base.T.t -> Base.T.t -> Base.T.t -> 'a) -> 'a
  val quantifyn : int -> (Base.T.t list -> 'a) -> 'a

  (** {2 The Datalog unit resolution algorithm} *)

  type t
  (** A database of facts and clauses, with incremental fixpoint computation *)

  type explanation =
    | Axiom
    | Resolution of Base.C.t * Base.Lit.t
    (** Explanation for a clause or fact. *)

  val create : unit -> t
    (** Create a DB *)

  val copy : t -> t
    (** Deep copy of the DB *)

  val mem : t -> Base.C.t -> bool
    (** Is the clause member of the DB? *)

  val add : ?expl:explanation -> t -> Base.C.t -> t
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val add_fact : ?expl:explanation -> t -> Base.Lit.t  -> t
    (** Add a fact (ground unit clause) *)

  val goal : t -> Base.Lit.t -> t
    (** Add a goal to the DB. The goal is used to trigger backward chaining
        (calling goal handlers that could help solve the goal) *)

  val match_ : t -> Base.Lit.t -> (Base.Lit.t -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match *)

  val query : t -> Base.Lit.t -> int list -> (const list -> unit) -> unit
    (** Like {!db_match}, but the additional int list is used to select
        bindings of variables in the literal. Their bindings, in the same
        order, are given to the callback. *)

  val size : t -> int
    (** Size of the DB *)

  val fold : ('a -> Base.C.t -> 'a) -> 'a -> t -> 'a
    (** Fold on all clauses in the current DB (including fixpoint) *)

  type fact_handler = Base.Lit.t -> unit
  type goal_handler = Base.Lit.t -> unit

  val subscribe_fact : t -> const -> fact_handler -> unit
  val subscribe_all_facts : t -> fact_handler -> unit
  val subscribe_goal : t -> goal_handler -> unit

  type user_fun = Base.Lit.t -> Base.Lit.t

  val add_fun : t -> const -> user_fun -> t
    (** Add a function to be called on new literals. Only one function per
        symbol can be registered. *)

  val goals : t -> (Base.Lit.t -> unit) -> unit
    (** Iterate on all current goals *)

  val explain : t -> Base.Lit.t -> Base.Lit.t list
    (** Explain the given fact by returning a list of facts that imply it
        under the current clauses, or raise Not_found *)

  val premises : t -> Base.Lit.t -> Base.C.t * Base.Lit.t list
    (** Immediate premises of the fact (ie the facts that resolved with
        a clause to give the literal), plus the clause that has been used. *)

  val explanations : t -> Base.C.t -> explanation list
    (** Get all the explanations that explain why this clause is true *)

  (** {2 Querying} *)

  module Query : sig
    type set
      (** mutable set of term lists *)

    val ask : t -> ?neg:Base.Lit.t list -> int array -> Base.Lit.t list -> set
      (** Given a list of variables, and a list of literals that contain those
          variables, return a set. Each element of the set is an instantiation
          of the variables such that all instantiated literals are facts of
          the [db]. [neg] is an optional list of literals that must be false
          for an instantiation to be an answer.
          This is lazy, and will only be evaluated upon calls to {! iter},
          {! to_list} or other similar functions. The answers will be cached
          in the set and readily available thereafter. *)

    val iter : set -> (Base.T.t array -> unit) -> unit
      (** Evaluate the set by iterating on it *)

    val to_list : set -> Base.T.t array list
      (** Convert to a list *)

    val cardinal : set -> int
      (** Number of elements of the set *)

    val pp_plan : Format.formatter -> set -> unit
      (** Print query plan *)
  end
end

(** Build a Datalog module *)
module Make(Base : Base.S) = struct
  module Base = Base
  module T = Base.T
  module C = Base.C
  module Lit = Base.Lit
  module Unif = Base.Unif

  (* ----------------------------------------------------------------------
   * The datalog bipartite resolution algorithm
   * ---------------------------------------------------------------------- *)

  exception UnsafeClause

  module ClausesIndex = Base.Index(struct
    type t = C.t
    let equal = Unif.clause_are_alpha_equiv
    let hash = C.hash
  end)

  module GoalIndex = Base.Index(struct
    type t = unit
    let equal a b = true
    let hash a = 0
  end)

  (** Explanation for a clause or fact. It is extensible through universal types. *)
  type explanation =
    | Axiom
    | Resolution of C.t * Base.Lit.t

  type fact_handler = Base.Lit.t -> unit
  type goal_handler = Base.Lit.t -> unit

  type user_fun = Lit.t -> Lit.t

  type queue_item =
    [ `AddClause of C.t * explanation
    | `AddGoal of Base.Lit.t
    ]

  module ConstTbl = Hashtbl.Make(Base.Const)

  (** A database of facts and clauses, with incremental fixpoint computation *)
  type t = {
    db_all : explanation C.Tbl.t;  (** maps all clauses to their explanations *)
    mutable db_facts : ClausesIndex.t;       (** index on facts *)
    mutable db_goals : GoalIndex.t;          (** set of goals *)
    mutable db_selected : ClausesIndex.t;    (** index on clauses' selected premises *)
    db_heads : ClausesIndex.t;                    (** index on clauses' heads *)
    db_fact_handlers : fact_handler ConstTbl.t;   (** map symbol -> fact handlers *)
    mutable db_all_facts : fact_handler list;
    mutable db_goal_handlers : goal_handler list; (** goal handlers *)
    db_funs : Base.BuiltinFun.map;           (** user-defined functions *)
    db_queue : queue_item Queue.t;           (** queue of items to process *)
  }

  type db = t

  (** Create a DB *)
  let create () =
    { db_all = C.Tbl.create 17;
      db_facts = ClausesIndex.empty ();
      db_goals = GoalIndex.empty ();
      db_selected = ClausesIndex.empty ();
      db_heads = ClausesIndex.empty ();
      db_all_facts = [];
      db_fact_handlers = ConstTbl.create 3;
      db_goal_handlers = [];
      db_funs = Base.BuiltinFun.create();
      db_queue = Queue.create ();
    }

  let copy db =
    { db_all = C.Tbl.copy db.db_all;
      db_facts = ClausesIndex.copy db.db_facts;
      db_goals = GoalIndex.copy db.db_goals;
      db_selected= ClausesIndex.copy db.db_selected;
      db_heads = ClausesIndex.copy db.db_heads;
      db_all_facts = db.db_all_facts;
      db_fact_handlers = ConstTbl.copy db.db_fact_handlers;
      db_goal_handlers = db.db_goal_handlers;
      db_funs = Base.BuiltinFun.copy db.db_funs;
      db_queue = Queue.create ();
    }

  (** Is the clause member of the DB? *)
  let mem db clause =
    C.Tbl.mem db.db_all clause

  (** Apply user-defined functions to the clause *)
  let rewrite_clause db clause =
    Base.BuiltinFun.eval_clause db.db_funs clause

  let resolve ?(oc=true) fact clause =
    match clause.C.body with
    | (Base.Lit.LitPos lit) :: body' ->
      begin try
        let subst = Base.Unif.unify fact 0 lit 1 in
        let renaming = Base.Subst.create_renaming () in
        Some (C.mk_clause
          (Base.Subst.eval subst ~renaming clause.C.head 1)
          (Base.Subst.eval_lits subst ~renaming body' 1)
        )
      with Base.Unif.Fail -> None
      end
    | _ -> None

  let add_clause db clause explanation =
    let clause = rewrite_clause db clause in
    (* check if clause already present; in any case add the explanation *)
    let already_present = mem db clause in
    C.Tbl.add db.db_all clause explanation;
    if already_present then ()
    (* generate new clauses by resolution *)
    else if C.is_fact clause then begin
      db.db_facts <- ClausesIndex.add db.db_facts clause.C.head clause;
      (* FIXME  handlers
      (* call handler for this fact, if any *)
      let call_handler h =
        try h clause.C.head
        with e ->
          Format.eprintf
            "Datalog: exception while calling handler for %s@."
            (Base.Const.to_string s);
          raise e
      in
      begin match C.head_symbol clause with
        | Some s -> 
          List.iter call_handler (ConstTbl.find_all db.db_fact_handlers s);
        | None -> ()
      end;
      List.iter call_handler db.db_all_facts;
      *)
      (* insertion of a fact: resolution with all clauses whose
         first body literal matches the fact. No offset is needed, because
         the fact is ground. *)
      ClausesIndex.generalizations db.db_selected 0 clause.C.head 0
        (fun clause' subst ->
          (* subst(clause'.(1)) = clause.(0) , remove the first element of the
             body of subst(clause'), that makes a new clause *)
          let clause'' = C. remove_first_subst subst (clause',0) in
          let explanation = Resolution (clause', clause.(0)) in
          Queue.push (`AddClause (clause'', explanation)) db.db_queue)
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
  let db_add ?(expl=Axiom) db clause =
    process_items db (`AddClause (clause, expl))

  (** Add a fact (ground unit clause) *)
  let db_add_fact ?(expl=Axiom) db lit =
    (if not (is_ground lit) then raise UnsafeClause);
    process_items db (`AddClause ([|lit|], expl))

  (** Add a goal to the DB. The goal is used to trigger backward chaining
      (calling goal handlers that could help solve the goal) *)
  let db_goal db lit =
    process_items db (`AddGoal lit)

  (** match the given literal with facts of the DB, calling the handler on
      each fact that match (with the corresponding substitution) *)
  let db_match db pattern handler =
    ClausesIndex.retrieve_specializations
      (fun () fact _ subst -> handler fact)
      () (db.db_facts,0) (pattern,1)

  (** Like {!db_match}, but the additional int list is used to select
      bindings of variables in the literal. Their bindings, in the same
      order, are given to the callback. *)
  let db_query db pattern vars k =
    ClausesIndex.retrieve_specializations
      (fun () lit _ subst ->
        let terms = List.map
          (fun i ->
            let v = mk_var i in
            let t, _ = deref subst v 1 in
            match t with 
            | Var _ -> assert false  (* should be ground *)
            | Const s -> s)
          vars in
        (* yield the list of terms *)
        k terms)
      () (db.db_facts,0) (pattern,1)

  (** Size of the DB *)
  let db_size db =
    ClausesIndex.size db.db_facts + ClausesIndex.size db.db_selected

  (** Fold on all clauses in the current DB (including fixpoint) *)
  let db_fold k acc db =
    C.Tbl.fold
      (fun clause _ acc -> k acc clause)
      db.db_all acc

  let db_add_fun db s f =
    (if ConstTbl.mem db.db_funs s
      then failwith ("function already defined for symbol " ^ Symbol.to_string s));
    ConstTbl.replace db.db_funs s f

  let db_subscribe_fact db symbol handler =
    ConstTbl.add db.db_fact_handlers symbol handler

  let db_subscribe_all_facts db handler =
    db.db_all_facts <- handler :: db.db_all_facts

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
    let explored = C.Tbl.create 5 in
    let set = LitHashtbl.create 5 in
    (* recursively collect explanations *)
    let rec search clause =
      if C.Tbl.mem explored clause then ()
      else begin
        C.Tbl.add explored clause ();
        let explanation = C.Tbl.find db.db_all clause in
        match explanation with
        | Axiom when is_fact clause ->
          LitHashtbl.replace set clause.(0) ();
        | ExtExplanation _
        | Axiom -> ()
        | Resolution (clause, fact) -> begin
          search clause;
          search [|fact|]
        end
      end
    in
    (* once the set is collected, convert it to list *)
    search [|fact|];
    LitHashtbl.fold (fun lit () acc -> lit :: acc) set []

  (** Immediate premises of the fact (ie the facts that resolved with
      a clause to give the literal), plus the clause that has been used. *)
  let db_premises db fact =
    let rec search acc clause =
      let explanation = C.Tbl.find db.db_all clause in
      match explanation with
      | ExtExplanation _
      | Axiom -> clause, acc  (* no premises *)
      | Resolution (clause, fact) -> let acc = fact :: acc in search acc clause
    in
    search [] [|fact|]

  (** Get all the explanations that explain why this clause is true *)
  let db_explanations db clause =
    C.Tbl.find_all db.db_all clause

  (** {2 Querying} *)

  module Query = struct
    module RowTable = Hashtbl.Make(struct
      type t = term array
      let equal = eq_literal
      let hash = hash_literal
    end)

    type set = {
      db : db;
      query : query;
    } (** mutable set of term lists *)
    and query = {
      q_expr : expr;                    (* relational expression *)
      q_vars : int array;               (* variables *)
      mutable q_table : table option;   (* answer table *)
    } (** A query *)
    and expr =
      | Match of literal * int array * int array  (* match with literal, then project *)
      | Join of query * query                 (* join on common variables *)
      | ProjectJoin of int array * query * query  (* join and project immediately *)
      | Project of int array * query            (* project on given columns*)
      | AntiJoin of query * query               (* tuples of q1 that are not joinable with q2 *)
    and table = {
      tbl_vars : int array;             (* vars labelling columns *)
      tbl_rows : unit RowTable.t;       (* set of rows *)
    } (** A relational table; column are labelled with variables *)
    and index = (int array, term array list) Hashtbl.t
      (** Index for a table. It indexes the given list of variables *)

    (* union of two sets of variable *)
    let union_vars l1 l2 =
      let l = Array.fold_left
        (fun acc x -> if List.mem x acc then acc else x :: acc)
        (Array.to_list l2) l1
      in
      Array.of_list (List.sort compare l)

    (* variables that are common to [l1] and [l2] *)
    let common_vars l1 l2 =
      let l2 = Array.to_list l2 in
      let l = Array.fold_left
        (fun acc x -> if List.mem x l2 then x :: acc else acc)
        [] l1
      in
      Array.of_list l

    (* build a query from an expression *)
    let mk_query expr =
      let q_vars = match expr with
      | Match (_, vars, _) -> vars
      | Join (q1, q2) ->
        if common_vars q1.q_vars q2.q_vars = [||]
          then Array.append q1.q_vars q2.q_vars
          else union_vars q1.q_vars q2.q_vars
      | ProjectJoin (vars, _, _) -> vars
      | Project (vars, _) -> vars
      | AntiJoin (q1, q2) -> q1.q_vars
      in
      { q_expr = expr; q_table = None; q_vars; }

    let mk_table vars =
      { tbl_vars = vars; tbl_rows = RowTable.create 27; }

    let add_table tbl row =
      RowTable.replace tbl.tbl_rows row ()

    let iter_table tbl k =
      RowTable.iter (fun row () -> k row) tbl.tbl_rows

    let length_table tbl = RowTable.length tbl.tbl_rows

    (* list of (var, index) for each variables in literal *)
    let vars_index_of_lit lit =
      let vars, indexes, _ = Array.fold_left
        (fun (vars,indexes,idx) t -> match t with
          | Var i when not (List.mem i vars) ->
            i::vars, idx::indexes, idx+1
          | _ -> vars, indexes, idx+1)
        ([], [], 0) lit
      in
      Array.of_list vars, Array.of_list indexes

    (* optimize query (TODO more optimization, e.g. re-balance joins) *)
    let rec optimize q = match q.q_expr with
      | Project (vars, {q_expr=Join(q1,q2)}) ->
        let q1 = optimize q1 in
        let q2 = optimize q2 in
        mk_query (ProjectJoin (vars, q1, q2))
      | Project (vars, q') ->
        if vars = q'.q_vars
          then optimize q'  (* reord is the identity *)
          else mk_query (Project (vars, optimize q'))
      | ProjectJoin (vars, q1, q2) ->
        let q1 = optimize q1 in
        let q2 = optimize q2 in
        mk_query (ProjectJoin (vars, q1, q2))
      | Join (q1, q2) -> mk_query (Join (optimize q1, optimize q2)) 
      | AntiJoin (q1, q2) ->
        let q1 = optimize q1 in
        let q2 = optimize q2 in
        if common_vars q1.q_vars q2.q_vars = [||]
          then q1 (* diff is trivial *)
          else (* TODO: try to push antijoins in q1? *)
            mk_query (AntiJoin (optimize q1, optimize q2))
      | Match _ -> q

    (** Given a list of variables, and a list of literals that contain those
        variables, return a set. Each element of the set is an instantiation
        of the variables such that all instantiated literals are facts of
        the [db].
        This is lazy, and will only be evaluated upon calls to {! iter},
        {! to_list} or other similar functions. *)
    let ask db ?(neg=[]) vars lits =
      assert (Array.length vars > 0);
      (* buid query for a given lit *)
      let rec build_query lit =
        let vars, indexes = vars_index_of_lit lit in
        mk_query (Match (lit, vars, indexes))
      (* combine queries. [vars] is the list of variables in [q]. [lits']
          are the remaining constraint literals *)
      and combine_queries q lits = match lits with
        | [] -> q
        | lit::lits' ->
          let q' = build_query lit in
          let q'' = mk_query (Join (q, q')) in
          combine_queries q'' lits'
      in
      let q = match lits with
      | [] -> failwith "Datalog.Query.ask: require at least one literal"
      | lit::lits' ->
        (* initial query, to combine with other queries *)
        let q_lit = build_query lit in
        combine_queries q_lit lits'
      in
      (* negations *)
      let q = match neg with
      | [] -> q
      | lit::lits ->
        let q_neg = build_query lit in
        let q_neg = combine_queries q_neg lits in
        mk_query (AntiJoin (q, q_neg))
      in
      (* project columns *)
      let q = mk_query (Project (vars, q)) in
      (* optimize *)
      let q = optimize q in
      (* return set *)
      { db; query = q; }

    (* select the given indexes of [a] *)
    let select_indexes indexes a =
      Array.map (fun i -> Array.get a i) indexes

    exception Found of int

    (* column indexes of variables in [l]. For each [v] in [vars], finds
        the index of the first occurrence of [v] in [l]. *)
    let find_indexes vars l =
      Array.map (fun v ->
        try
          Array.iteri (fun i v' -> if v = v' then raise (Found i)) l;
          raise Not_found
        with Found i ->
          i)
      vars

    (** Evaluate the query set *)
    let rec eval db query =
      match query.q_table with
      | Some l -> l
      | None ->
        let tbl = match query.q_expr with
        | Match (lit, vars, indexes) ->
          let tbl = mk_table vars in
          (* iterate on literals that match [lit] *)
          db_match db lit
            (fun lit' ->
              let row = project indexes lit' in
              add_table tbl row);
          tbl
        | Project (vars, q) ->
          let tbl = eval db q in
          (* map variables to their index in the input table *)
          let indexes = find_indexes vars tbl.tbl_vars in
          let result = mk_table vars in
          (* project each input tuple *)
          iter_table tbl
            (fun row ->
              let row' = project indexes row in
              add_table result row');
          result
        | ProjectJoin (vars, q1, q2) ->
          eval_join ~vars db q1 q2
        | Join (q1, q2) ->
          eval_join ?vars:None db q1 q2
        | AntiJoin (q1, q2) ->
          let tbl1 = eval db q1 in
          let tbl2 = eval db q2 in
          antijoin tbl1 tbl2
      in
      (* save result before returning it *)
      query.q_table <- Some tbl;
      tbl
    (* special case of joins *)
    and eval_join ?vars db q1 q2 =
      (* evaluate subqueries *)
      let tbl1 = eval db q1 in
      let tbl2 = eval db q2 in
      (* common variables *)
      let common = common_vars tbl1.tbl_vars tbl2.tbl_vars in
      (* project on which vars? *)
      match vars, common with
      | None, [||] -> product tbl1 tbl2
      | Some vars, [||] -> project_product ~vars tbl1 tbl2
      | None, _ ->
        let vars = union_vars tbl1.tbl_vars tbl2.tbl_vars in
        join ~vars common tbl1 tbl2
      | Some vars, _ ->
        join ~vars common tbl1 tbl2
    (* project on the given indexes *)
    and project indexes row =
      Array.map (fun i -> row.(i)) indexes
    (* cartesian product *)
    and product tbl1 tbl2 =
      let vars = Array.append tbl1.tbl_vars tbl2.tbl_vars in
      let tbl = mk_table vars in
      iter_table tbl1
        (fun row1 ->
          iter_table tbl2
            (fun row2 -> let row = Array.append row1 row2 in add_table tbl row));
      tbl
    (* projection + cartesian product *)
    and project_product ~vars tbl1 tbl2 =
      let tbl = mk_table vars in
      let indexes = find_indexes vars (Array.append tbl1.tbl_vars tbl2.tbl_vars) in
      iter_table tbl1
        (fun row1 ->
          iter_table tbl2
            (fun row2 ->
              let row = Array.append row1 row2 in
              let row = project indexes row in  (* project *)
              add_table tbl row));
      tbl
    (* join on the given list of common variables, and project on [vars] *)
    and join ~vars common tbl1 tbl2 =
      let vars1 = tbl1.tbl_vars and vars2 = tbl2.tbl_vars in
      (* bij: var -> index of var in joined columns *)
      let indexes = find_indexes vars (Array.append vars1 vars2) in
      let result = mk_table vars in
      (* index rows of [tbl1] *)
      let idx1 = mk_index tbl1 common in
      (* which column in [tbl2] for variables of [common]? *)
      let common_indexes = find_indexes common vars2 in
      (* join on [tbl2] *)
      iter_table tbl2
        (fun row2 ->
          let join_items = select_indexes common_indexes row2 in
          (* join on [join_items] *)
          let rows1 = try Hashtbl.find idx1 join_items with Not_found -> [] in
          List.iter
            (fun row1 ->
              let row = project indexes (Array.append row1 row2) in
              add_table result row)
            rows1);
      result
    (* Antijoin of tables *)
    and antijoin tbl1 tbl2 =
      (* common variables *)
      let common = common_vars tbl1.tbl_vars tbl2.tbl_vars in
      assert (common <> [||]);
      let common_indexes = find_indexes common tbl1.tbl_vars in
      (* index tbl2, to test rows of tbl1 *)
      let idx2 = mk_index tbl2 common in
      let result = mk_table tbl1.tbl_vars in
      iter_table tbl1
        (fun row ->
          let join_items = select_indexes common_indexes row in
          if Hashtbl.mem idx2 join_items
            then ()  (* drop row *)
            else add_table result row);
      result
    (* Build an index for the given list of variables (in this order). The
        indexed rows do not contain the selected columns. *)
    and mk_index tbl vars =
      let indexes = find_indexes vars tbl.tbl_vars in
      let h = Hashtbl.create 17 in
      iter_table tbl
        (fun row ->
          (* tuple of values to index with *)
          let indexed_items = select_indexes indexes row in
          (* add [row] to the rows indexed by the same items *)
          let rows = try Hashtbl.find h indexed_items with Not_found -> [] in
          Hashtbl.replace h indexed_items (row::rows));
      h

    (** Evaluate the set and iter on it *)
    let iter set k =
      let answers = eval set.db set.query in
      iter_table answers k

    let to_list set =
      let tbl = eval set.db set.query in
      let l = ref [] in
      iter_table tbl (fun row -> l := row :: !l);
      !l

    let cardinal set =
      let tbl = eval set.db set.query in
      length_table tbl

    let pp_array ~sep pp_elt fmt a =
      Array.iteri (fun i x ->
        (if i > 0 then Format.pp_print_string fmt sep);
        pp_elt fmt x)
      a

    let pp_plan formatter set =
      let pp_var fmt i = Format.pp_print_int fmt i in
      let rec pp_q ~top fmt q = match q.q_expr with
      | Match (lit, vars, _) ->
        Format.fprintf fmt "match[%a] %a" (pp_array ~sep:"," pp_var) vars pp_literal lit
      | Join (q1, q2) ->
        (if not top then Format.pp_print_string fmt "(");
        Format.fprintf fmt "%a |><| %a" (pp_q ~top:false) q1 (pp_q ~top:false) q2;
        (if not top then Format.pp_print_string fmt ")");
      | ProjectJoin (vars, q1, q2) ->
        (if not top then Format.pp_print_string fmt "(");
        Format.fprintf fmt "%a |><|_[%a] %a" (pp_q ~top:false) q1
          (pp_array ~sep:"," pp_var) vars (pp_q ~top:false) q2;
        (if not top then Format.pp_print_string fmt ")");
      | AntiJoin (q1, q2) ->
        Format.fprintf fmt "%a |> %a" (pp_q ~top:false) q1 (pp_q ~top:false) q2
      | Project (vars, q) ->
        Format.fprintf fmt "project[%a] %a" (pp_array ~sep:"," pp_var) vars (pp_q ~top:false) q
      in pp_q ~top:true formatter set.query
  end
end
