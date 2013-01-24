(** Representation for Datalog terms and rules, and the main algorithm *)

(** Module type for logic *)
module type S = sig
  (* ----------------------------------------------------------------------
   * Terms and rules
   * ---------------------------------------------------------------------- *)

  type symbol
    (** Abstract type of symbols *)

  type term
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
        array is the predicate, then arguments follow *)

  type rule
    (** A datalog rule, i.e. head :- body_1, ..., body_n. [n] must be <= 31. *)

  type subst
    (** A substitution maps variables to symbols *)

  val mk_term : symbol -> [`Var of int | `Symbol of symbol] list -> term
    (** Helper to build a term. Arguments are either variables or symbols; if they
        variables indexes *must* be negative (otherwise it will raise Invalid_argument *)

  val mk_term_s : string -> [`Var of int | `Symbol of string] list -> term
    (** Same as [mk_term], but converts strings to symbols on-the-fly *)

  val open_term : term -> symbol * [`Var of int | `Symbol of symbol] list
    (** Deconstruct a term *)

  val mk_rule : term -> (term * bool) list -> rule
    (** Create a rule from a conclusion and a list of body premises with their sign. *)

  val open_rule : rule -> term * (term * bool) list
    (** Deconstruct a rule *)

  val is_var : int -> bool
    (** A variable is a negative int *)

  val is_ground : term -> bool
    (** Is the term ground (a fact)? *)

  val arity : term -> int
    (** Number of subterms of the term. Ex for p(a,b,c) it returns 3 *)

  val eq_term : term -> term -> bool
    (** Are the terms equal? *)

  val hash_term : term -> int
    (** Hash the term *)

  val subst_term : subst -> term -> term
    (** Apply substitution to the term *)

  val subst_rule : subst -> rule -> rule
    (** Apply substitution to the rule *)

  val check_safe : rule -> bool
    (** A datalog rule is safe iff all variables in its head also occur in its body *)

  val is_fact : rule -> bool
    (** A fact is a ground rule with empty body *)

  val compare_rule : rule -> rule -> int
    (** Lexicographic comparison of rules *)

  val eq_rule : rule -> rule -> bool
    (** Check whether rules are (syntactically) equal *)

  val hash_rule : rule -> int
    (** Hash the rule *)

  val pp_term : Format.formatter -> term -> unit
    (** Pretty print the term *)

  val pp_rule : Format.formatter -> rule -> unit
    (** Pretty print the rule *)

  (* ----------------------------------------------------------------------
   * The datalog bipartite resolution algorithm
   * ---------------------------------------------------------------------- *)

  type db
    (** A database of facts and rules, with incremental fixpoint computation *)

  val db_create : unit -> db
    (** Create a DB *)

  val db_mem : db -> rule -> bool
    (** Is the rule member of the DB? *)

  val db_add : db -> rule -> unit
    (** Add the rule/fact to the DB as an axiom, updating fixpoint *)

  val db_remove : db -> rule -> unit
    (** Remove the rule/fact from the DB as an axiom. It may remove consequences
        of the rule. *)

  val db_match : db -> term -> (term -> subst -> unit) -> unit
    (** match the given term with facts of the DB, calling the handler on
        each fact that match (with the corresponding substitution) *)

  val db_size : db -> int
    (** Size of the DB *)

  val db_fold : ('a -> rule -> 'a) -> 'a -> db -> 'a
    (** Fold on all rules in the current DB (including fixpoint) *)

  val db_subscribe : db -> symbol -> (term -> bool -> unit) -> unit
    (** [db_subscribe db symbol handler] causes [handler] to be called with
        any added/removed fact that has head symbol [symbol] from now on.
        the bool indicates whether the fact is added (true) or removed (false). *)

  val db_explain : db -> term -> term list
    (** Explain the given fact by returning a list of facts that imply it
        under the current rules. *)

  val db_premises : db -> term -> rule * term list
    (** Immediate premises of the fact (ie the facts that resolved with
        a clause to give the term), plus the rule that has been used. *)
end

(** Signature for a symbol type. It must be hashable, comparable and
    in bijection with strings *)
module type SymbolType = sig
  include Hashtbl.HashedType
  val to_string : t -> string
  val of_string : string -> t
end

module Make(Symbol : SymbolType) = struct
  (* ----------------------------------------------------------------------
   * Terms and rules
   * ---------------------------------------------------------------------- *)

  type symbol = Symbol.t
    (** Abstract type of symbols *)

  module SymbolHashtbl = Hashtbl.Make(Symbol)

  (* bijective mapping int <-> symbols *)
  let __i_to_s = Utils.IHashtbl.create 5
  let __s_to_i = SymbolHashtbl.create 5
  let __symbol_count = ref 0

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
  let i_to_s i = Utils.IHashtbl.find __i_to_s i

  (** Forget about the symbol. If the corresponding int [i] it still used,
      [get_symbol i] will fail with Not_found. *)
  let rm_symbol s =
    try
      let i = SymbolHashtbl.find __s_to_i s in
      Utils.IHashtbl.remove __i_to_s i;
      SymbolHashtbl.remove __s_to_i s
    with Not_found -> ()

  type term = int array
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
        array is the predicate, then arguments follow *)

  type rule = term array
    (** A datalog rule, i.e. head :- body_1, ..., body_n. The actual representation
        if [| head; [| bitvector |]; body_1; ...; body_n |]. The bitvector
        stores the sign of body literals (head is always positive). *)

  type subst = int Utils.IHashtbl.t
    (** A substitution is a map from (negative) ints to (positive) ints *)

  (** Helper to build a term. Arguments are either variables or symbols; if they
      are variables, the int must be negative. *)
  let mk_term head args =
    let head = s_to_i head in
    let args = List.map
      (function
       | `Var i -> assert (i < 0); i
       | `Symbol s -> s_to_i s)
      args in
    Array.of_list (head :: args)

  (** Same as [mk_term], but converts strings to symbols on-the-fly *)
  let mk_term_s head args =
    let head = Symbol.of_string head in
    let args = List.map
      (function
      | `Var i -> `Var i
      | `Symbol s -> `Symbol (Symbol.of_string s))
      args in
    mk_term head args

  (** Deconstruct a term *)
  let open_term term =
    let head = term.(0) in
    let head = i_to_s head in
    let args = Array.to_list (Array.sub term 1 (Array.length term - 1)) in
    let args = List.map
      (fun i -> if i < 0 then `Var i else `Symbol (i_to_s i))
      args in
    head, args

  (** Sign of the i-th literal of a rule *)
  let sign_lit rule i =
    assert (i < Array.length rule - 2);
    let bv = rule.(1).(0) in
    Bitvector.bv_get bv i

    (** Create a rule from a conclusion and a list of body premises with their sign. *)
  let mk_rule head premises = 
    assert (List.length premises <= 31);
    if premises = [] then [|head|]
    else
      let bv, _ = List.fold_left
        (fun (bv, i) (_, sign) -> 
          let bv = if sign then Bitvector.bv_set bv i else bv in
          bv, i+1)
        (0, 0) premises
      in
      Array.of_list (head :: [| bv |] :: List.map fst premises)

  (** Deconstruct a rule *)
  let open_rule rule =
    let head = rule.(0) in
    if Array.length rule = 1 then head, [] else
    let body = Array.sub rule 2 (Array.length rule - 2) in
    let body = Array.mapi (fun i lit -> lit, sign_lit rule i) body in
    let body = Array.to_list body in
    head, body

  (** A variable is a negative int *)
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

  (** compare terms *)
  let compare_term = Utils.compare_ints

  (** Are the terms equal? *)
  let eq_term t1 t2 = compare_term t1 t2 = 0

  (** Hash the term *)
  let hash_term t = Utils.hash_ints t

  (** Apply substitution to the term *)
  let subst_term subst t =
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

  (** Apply substitution to the rule. TODO remove duplicate literals afterward *)
  let subst_rule subst rule =
    if Utils.IHashtbl.length subst = 0 then rule
    else begin
      let a = Array.copy rule in
      a.(0) <- subst_term subst a.(0);
      for i = 2 to Array.length rule - 1 do
        a.(i) <- subst_term subst a.(i);
      done;
      a
    end

  (** A datalog rule is safe iff all variables in its head also occur in
      positive literals of its body *)
  let check_safe rule =
    let rec check_head i =
      if i = Array.length rule.(0) then true
      else
        let t = rule.(0).(i) in
        if is_var t
          then check_body t 2 && check_head (i+1)
          else check_head (i+1)
    and check_body var j =
      if j = Array.length rule then false
        (* check occurrence of var in j-th literal, if the literal is positive *)
        else (sign_lit rule (j-2) && check_body_term var rule.(j) 1)
           || check_body var (j+1)
    and check_body_term var term k =
      if k = Array.length term then false
      else if term.(k) = var then true
      else check_body_term var term (k+1)
    in
    check_head 2

  (** A fact is a ground rule with empty body *)
  let is_fact rule = Array.length rule = 1 && is_ground rule.(0)

  let compare_rule r1 r2 =
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

  (** Check whether rules are (syntactically) equal *)
  let eq_rule r1 r2 = compare_rule r1 r2 = 0

  (** Hash the rule *)
  let hash_rule r =
    let h = ref 17 in
    for i = 0 to Array.length r - 1 do
      h := (!h + 65536) * hash_term r.(i);
    done;
    abs !h

  (** Remove first body element of the rule, after substitution *)
  let remove_first_subst subst rule =
    assert (Array.length rule > 1);
    let a = Array.make (Array.length rule - 1) [||] in
    a.(0) <- subst_term subst rule.(0);
    a.(1) <- [|Bitvector.bv_lsr rule.(1).(0) 1|];  (* remove least significant bit *)
    for i = 2 to Array.length rule - 2 do
      a.(i) <- subst_term subst rule.(i+1);
    done;
    a

  let pp_term formatter t =
    (* symbol index (int) to string *)
    let to_s s = Symbol.to_string (i_to_s s) in
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

  let pp_rule formatter rule =
    let pp_sign formatter i =
      if sign_lit rule (i-2) then () else Format.fprintf formatter "~"
    in
    if Array.length rule = 1
      then Format.fprintf formatter "%a." pp_term rule.(0)
      else begin
        Format.fprintf formatter "%a :-@ "  pp_term rule.(0);
        for i = 2 to Array.length rule - 1 do
          (if i > 2 then Format.fprintf formatter ",@ ");
          Format.fprintf formatter "%a%a" pp_sign i pp_term rule.(i);
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

      module DataSet : Set.S with type elt = elt
        (** Set of indexed elements *)

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
  module Make(X : Set.OrderedType) : Index with type elt = X.t =
    struct

      (** A set of indexed data *)
      module DataSet = Set.Make(X)

      (** The term index. It is a trie with, at each node, a hashset
          of elements, plus a map symbol/var -> subtrie *)
      type t = 
      | Node of DataSet.t ref * t Utils.IHashtbl.t  

      (** Indexed elements *)
      type elt = X.t

      (** Create a new index *)
      let create () = Node (ref DataSet.empty, Utils.IHashtbl.create 2)

      (** Add the element indexed by the term *)
      let add t term elt =
        let len = Array.length term in
        (* index in subtrie [t], with a cursor at term[i]. *)
        let rec add t i = match t, i with
        | Node (set, subtries), i when i = len ->
          set := DataSet.add elt !set (* insert in leaf *)
        | Node (_, subtries), i ->
          try
            let subtrie = Utils.IHashtbl.find subtries term.(i) in
            add subtrie (i+1)
          with Not_found ->
            (* create a new subtrie for the i-th argument of term, then recurse *)
            let subtrie = Node (ref DataSet.empty, Utils.IHashtbl.create 2) in
            Utils.IHashtbl.add subtries term.(i) subtrie;
            add subtrie (i+1)
        in
        add t 0

      (** Reset to empty index *)
      let clear t = match t with
        | Node (set, subtries) ->
          set := DataSet.empty;
          Utils.IHashtbl.clear subtries

      (** Fold on generalizations of given ground term (with transient substitution) *)
      let retrieve_generalizations k acc t term =
        assert (is_ground term);
        let subst = Utils.IHashtbl.create 2 in
        let len = Array.length term in
        (* search in subtrie [t], with cursor at [i]-th argument of [term] *)
        let rec search t i acc = match t, i with
        | Node (set, _), i when i = len ->
          DataSet.fold (fun elt acc -> k acc elt subst) !set acc
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
        let subst = Utils.IHashtbl.create 2 in
        let len = Array.length term in
        (* search in subtrie [t], with cursor at [i]-th argument of [term] *)
        let rec search t i acc = match t, i with
        | Node (set, _), i when i = len ->
          DataSet.fold (fun elt acc -> k acc elt subst) !set acc
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

  module RulesIndex = Make(
    struct
      type t = rule
      let compare = compare_rule
    end)

  (** Hashtable on rules *)
  module RuleHashtbl = Hashtbl.Make(
    struct
      type t = rule
      let equal = eq_rule
      let hash = hash_rule
    end)

  (** Explanation for a rule or fact *)
  type explanation =
    | Axiom
    | Resolution of rule * term

  (** A handler for addition/deletion of facts *)
  type fact_handler = term -> bool -> unit

  (** A database of facts and rules, with incremental fixpoint computation *)
  type db = {
    db_all : explanation list RuleHashtbl.t;        (** maps all rules to their explanations *)
    db_facts : RulesIndex.t;                        (** index on facts *)
    db_rules : RulesIndex.t;                        (** index on rules *)
    db_handlers : fact_handler Utils.IHashtbl.t;    (** map symbol -> handler *)
    db_queue : (rule * explanation) Queue.t;        (** queue of rules to add (+ explanation) *)
  }

  (** Create a DB *)
  let db_create () =
    { db_all = RuleHashtbl.create 17;
      db_facts = RulesIndex.create ();
      db_rules = RulesIndex.create ();
      db_handlers = Utils.IHashtbl.create 3;
      db_queue = Queue.create ();
    }

  (** Is the rule member of the DB? *)
  let db_mem db rule =
    assert (check_safe rule);
    RuleHashtbl.mem db.db_all rule

  (* TODO: need to remember inferences a rule took part in, to remove the explanations
     (and maybe other rules that were implied only by those) upon removal of the rule.
     Maybe a doubly-linked list of explanations, shared by conclusion+2 premises *)

  (** Add the rule/fact to the DB, updating fixpoint *)
  let db_add db rule =
    assert (check_safe rule);
    (* queue of new rules to insert *)
    let queue = db.db_queue in
    (* is there already a add() going on? *)
    let already_active = not (Queue.is_empty queue) in
    (* add [rule] to the queue of rules to add *)
    Queue.push (rule, Axiom) queue;
    (* if there is already a add() going on, let it propagate the rule *)
    if already_active then () else
    while not (Queue.is_empty queue) do
      let rule, explanation = Queue.take queue in
      (* FIXME if already present, still add an explanation *)
      if db_mem db rule then () else begin
      (* rule not already present, add it *)
      RuleHashtbl.replace db.db_all rule [explanation];
      (* generate new rules by resolution *)
      if is_fact rule
      then begin
        RulesIndex.add db.db_facts rule.(0) rule;
        (* call handler for this fact, if any *)
        (try let handler = Utils.IHashtbl.find db.db_handlers rule.(0).(0)
             in handler rule.(0) true
        with
        | Not_found -> ()
        | e -> (
          Format.eprintf "Datalog: exception while calling handler for %d@." rule.(0).(0);
          raise e));
        (* insertion of a fact: resolution with all rules whose first body term
           is positive and matches the fact *)
        RulesIndex.retrieve_generalizations
          (fun () rule' subst ->
            if not (is_fact rule') then 
              (* rule' is not a fact, and
                 subst(rule'.body.(0)) = fact, remove the first element of the
                 body of rule', that makes a new rule *)
              let rule'' = remove_first_subst subst rule' in
              let explanation = Resolution (rule', rule.(0)) in
              Queue.push (rule'', explanation) queue)
          () db.db_rules rule.(0)
      end else if sign_lit rule 0 then begin
        (* first body literal is positive *)
        assert (Array.length rule > 2);
        RulesIndex.add db.db_rules rule.(2) rule;
        (* insertion of a non_unit rule: resolution with all facts that match the
           first body term of the rule *)
        RulesIndex.retrieve_specializations
          (fun () fact subst ->
            (* subst(rule.body.(0)) = fact, remove this first literal *)
            let rule' = remove_first_subst subst rule in
            let explanation = Resolution (rule, fact.(0)) in
            Queue.push (rule', explanation) queue)
          () db.db_facts rule.(2)
      end else begin
        (* first body literal is negative TODO *)
        failwith "not implemented"
      end
      end
    done

  (** Remove the rule/fact from the DB as an axiom. It may remove consequences
      of the rule. *)
  let db_remove db rule = failwith "not implemented"

  (** match the given term with facts of the DB, calling the handler on
      each fact that match (with the corresponding substitution) *)
  let db_match db pattern handler =
    RulesIndex.retrieve_specializations
      (fun () fact subst -> handler fact.(0) subst)
      () db.db_facts pattern

  (** Size of the DB *)
  let db_size db = RuleHashtbl.length db.db_all

  (** Fold on all rules in the current DB (including fixpoint) *)
  let db_fold k acc db =
    RuleHashtbl.fold
      (fun rule _ acc -> k acc rule)
      db.db_all acc

  (** [db_subscribe db symbol handler] causes [handler] to be called with
      any new fact that has head symbol [symbol] from now on *)
  let db_subscribe db symbol handler =
    let i = s_to_i symbol in
    Utils.IHashtbl.replace db.db_handlers i handler

  (** Explain the given fact by returning a list of facts that imply it
      under the current rules. *)
  let db_explain db fact =
    let module TermSet = Set.Make(struct type t = term let compare = compare_term end) in
    let explored = ref RulesIndex.DataSet.empty
    and set = ref TermSet.empty in
    (* recursively collect explanations *)
    let rec search rule =
      if RulesIndex.DataSet.mem rule !explored then ()
      else begin
        explored := RulesIndex.DataSet.add rule !explored;
        let explanation = RuleHashtbl.find db.db_all rule in
        match explanation with
        | Axiom::_ when is_fact rule -> set := TermSet.add rule.(0) !set
        | Axiom::_ -> ()
        | Resolution (rule, fact)::_ -> begin
          search rule;
          search [|fact|]
        end
        | [] -> assert false
      end
    in
    (* once the set is collected, convert it to list *)
    search [|fact|];
    TermSet.elements !set

  (** Immediate premises of the fact (ie the facts that resolved with
      a clause to give the term), plus the rule that has been used. *)
  let db_premises db fact =
    let rec search acc rule =
      let explanation = RuleHashtbl.find db.db_all rule in
      match explanation with
      | Axiom::_ -> rule, acc  (* no premises *)
      | Resolution (rule, fact)::_ -> let acc = fact :: acc in search acc rule
      | [] -> assert false
    in
    search [] [|fact|]
end

(** Default term base, where symbols are just strings *)
module Default = Make(
  struct
    type t = string
    let to_string s = s
    let of_string s = s
    let equal s1 s2 = String.compare s1 s2 = 0
    let hash s = Hashtbl.hash s
  end)
