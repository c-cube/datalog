
(* this file is part of datalog. See README for the license *)

(** {1 Bridge between Datalog.TopDown and OCaml} *)

module TopDown = Datalog_top_down

(** {2 Constants with universal types} *)

module Univ = struct
  type t =
  | Store : 'a key * 'a * (unit -> unit) -> t

  and 'a key = {
    mutable content : 'a option;
    eq : 'a -> 'a -> bool;
    hash : 'a -> int;
    print : 'a -> string;
  }

  let _print_default _ = "<opaque>"

  let new_key ?(eq=(=)) ?(hash=Hashtbl.hash) ?(print=_print_default) () =
    { content=None; eq; hash; print; }

  let pack ~key x =
    let get () = key.content <- Some x in
    Store (key, x, get)

  let unpack ~key u = match u with
    | Store (_, _, get) ->
      key.content <- None;
      get ();
      key.content

  let compatible ~key u =
    match unpack ~key u with | Some _ -> true | None -> false

  let eq u1 u2 =
    match u1 with
    | Store (key1, x1, _) ->
      match unpack ~key:key1 u2 with
      | None -> false
      | Some x2 -> key1.eq x1 x2

  let hash u = match u with
    | Store (key, x, _) -> key.hash x

  let print u = match u with
    | Store (key, x, _) -> key.print x

  let _id x = x
  let _const x _ = x

  let string = new_key
    ~eq:(fun s1 s2 -> s1=s2)
    ~hash:(fun s -> Hashtbl.hash s)
    ~print:_id ()

  let int = new_key
    ~hash:(fun i -> i land max_int)
    ~eq:(fun i j -> i=j)
    ~print:string_of_int ()

  let bool = new_key
    ~eq:(fun i j -> i=j)
    ~hash:(function true -> 1 | false -> 2)
    ~print:string_of_bool ()

  let float = new_key
    ~eq:(fun i j -> i=j)
    ~print:string_of_float ()

  let unit = new_key ~print:(_const "()") ()

  let pair a b =
    new_key
      ~eq:(fun (a1,b1)(a2,b2) -> a.eq a1 a2 && b.eq b1 b2)
      ~hash:(fun (a1,b1) -> a.hash a1 * 65993 + b.hash b1)
      ~print:(fun (a1,b1) -> Printf.sprintf "(%s,%s)" (a.print a1) (b.print b1))
      ()

  let list a =
    let _print_list l =
      let buf = Buffer.create 15 in
      Buffer.add_char buf '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (a.print x))
        l;
      Buffer.add_char buf ']';
      Buffer.contents buf
    in
    new_key
      ~eq:(fun l1 l2 -> try List.for_all2 a.eq l1 l2 with Invalid_argument _ -> false)
      ~hash:(fun l -> List.fold_left (fun h x -> 65993 * h + a.hash x) 13 l)
      ~print:_print_list
      ()

  let array k =
    let _print_arr l =
      let buf = Buffer.create 15 in
      Buffer.add_string buf "[|";
      Array.iteri
        (fun i x ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (k.print x))
        l;
      Buffer.add_string buf "|]";
      Buffer.contents buf
    and _eq a1 a2 =
      Array.length a1 = Array.length a2 && try
        for i = 0 to Array.length a1 - 1 do
          if not (k.eq a1.(i) a2.(i)) then raise Exit
        done;
        true
      with Exit -> false
    and _hash a =
      let h = ref 13 in
      for i = 0 to Array.length a - 1 do
        h := 65993 * !h + k.hash a.(i)
      done;
      !h
    in
    new_key
      ~eq:_eq ~hash:_hash ~print:_print_arr ()
end

(** Datalog constant *)
type const = Univ.t

let _key_query = Univ.new_key ~print:(fun () -> "query") ()
  (* special query symbol: unit, with a specific, hidden embedding *)

let of_string s = Univ.pack ~key:Univ.string s
let of_int i = Univ.pack ~key:Univ.int i

module Logic = TopDown.Make(struct
  type t = const
  let equal = Univ.eq
  let hash = Univ.hash
  let to_string = Univ.print
  let of_string = of_string
  let query = Univ.pack ~key:_key_query ()
end)

(** {2 Typed relations} *)

module T = Logic.T
module C = Logic.C
module DB = Logic.DB

module Rel1 = struct
  type 'a t = Univ.t * 'a Univ.key

  let name (n, _) =
    match Univ.unpack ~key:Univ.string n with
    | None -> assert false
    | Some s -> s

  let create ?(k=Univ.new_key ()) name = (of_string name, k)

  let get (name,k1) t = match t with
    | T.Apply (name', [| T.Apply(u1, [| |]) |]) when Univ.eq name name' ->
      Univ.unpack ~key:k1 u1
    | _ -> None

  let make (n,k) x =
    let a1 = T.mk_const (Univ.pack ~key:k x) in
    T.mk_apply n [| a1 |]

  let apply (n,_) t = T.mk_apply n [| t |]

  (* find instances of the relation *)
  let find db ((n,_k) as rel) =
    let query = T.mk_apply n [| T.mk_var 0 |] in
    let l = Logic.ask db query in
    List.fold_left
      (fun acc t -> match get rel t with
        | None -> acc
        | Some x -> x :: acc)
      [] l

  (* r1(X) => r2(X), so r1 subset of r2 *)
  let subset db (n1,_) (n2,_) =
    let x = T.mk_var 0 in
    let c = C.mk_clause
      (T.mk_apply n2 [|x|])
      [ Logic.Lit.mk_pos (T.mk_apply n1 [|x|]) ]
    in
    DB.add_clause db c

  let from_fun db ((n,_k) as rel) f =
    DB.interpret db n
      (fun t -> match get rel t with
        | None -> []
        | Some x ->
          if f x
            then [C.mk_fact t]  (* the fact is true, says [f] *)
            else [])

  let add_list db rel l =
    List.iter
      (fun t -> DB.add_fact db (make rel t))
      l

  let to_string t = name t ^ "/1"
  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end

module Rel2 = struct
  type ('a,'b) t = Univ.t * 'a Univ.key * 'b Univ.key

  let name (n, _, _) =
    match Univ.unpack ~key:Univ.string n with
    | None -> assert false
    | Some s -> s

  let create ?(k1=Univ.new_key ()) ?(k2=Univ.new_key ()) name =
    (of_string name, k1, k2)

  let get (name,k1,k2) t = match t with
    | T.Apply (name',
      [| T.Apply(u1, [| |])
      ;  T.Apply(u2, [| |])
      |]) when Univ.eq name name' ->
      begin match Univ.unpack ~key:k1 u1, Univ.unpack ~key:k2 u2 with
      | Some x1, Some x2 -> Some (x1,x2)
      | _ -> None
      end
    | _ -> None

  let make (n,k1,k2) x1 x2 =
    let a1 = T.mk_const (Univ.pack ~key:k1 x1) in
    let a2 = T.mk_const (Univ.pack ~key:k2 x2) in
    T.mk_apply n [| a1; a2 |]

  let apply (n,_,_) t1 t2 = T.mk_apply n [| t1; t2 |]

  (* find instances of the relation *)
  let find db ((n,_,_) as rel) =
    let query = T.mk_apply n [| T.mk_var 0; T.mk_var 1 |] in
    let l = Logic.ask db query in
    List.fold_left
      (fun acc t -> match get rel t with
        | None -> acc
        | Some (x,y) -> (x,y) :: acc)
      [] l

  (* r1 => r2, so r1 subset of r2 *)
  let subset db (n1,_,_) (n2,_,_) =
    let x, y = T.mk_var 0, T.mk_var 1 in
    let c = C.mk_clause
      (T.mk_apply n2 [|x;y|])
      [ Logic.Lit.mk_pos (T.mk_apply n1 [|x;y|]) ]
    in
    DB.add_clause db c

  (* r(X,Y) :- r(X,Z), r(Z,Y) *)
  let transitive db (n,_,_) =
    let x, y, z = T.mk_var 0, T.mk_var 1, T.mk_var 2 in
    let c = C.mk_clause
      (T.mk_apply n [|x; y|])
      [ Logic.Lit.mk_pos (T.mk_apply n [|x; z|])
      ; Logic.Lit.mk_pos (T.mk_apply n [|z; y|])
      ]
    in
    DB.add_clause db c

  (* tc(X,Y) <=> r*(X,Y) *)
  let tc_of db ~tc:((name_tc,_,_) as tc) ((name_r,_,_) as r) =
    let x, y, z = T.mk_var 0, T.mk_var 1, T.mk_var 2 in
    let c = C.mk_clause
      (T.mk_apply name_tc [|x; y|])
      [ Logic.Lit.mk_pos (T.mk_apply name_r [|x; z|])
      ; Logic.Lit.mk_pos (T.mk_apply name_tc [|z; y|])
      ]
    in
    DB.add_clause db c;
    subset db r tc;
    ()

  (* r(X,Y) :- X=Y *)
  let reflexive db (n,_,_) =
    let x, y = T.mk_var 0, T.mk_var 1 in
    let c = C.mk_clause
      (T.mk_apply n [|x;y|])
      [ Logic.Lit.mk_pos (T.mk_apply (of_string "=") [|x;y|]) ]
    in
    DB.add_clause db c

  (* r(X,Y) => r(Y,X) *)
  let symmetry db (n,_,_) =
    let x, y = T.mk_var 0, T.mk_var 1 in
    let c = C.mk_clause
      (T.mk_apply n [|x;y|])
      [ Logic.Lit.mk_pos (T.mk_apply n [|y;x|]) ]
    in
    DB.add_clause db c

  let from_fun db ((n,_,_) as rel) f =
    DB.interpret db n
      (fun t -> match get rel t with
        | None -> []
        | Some (x,y) ->
          if f x y
            then [C.mk_fact t]  (* the fact is true, says [f] *)
            else [])

  let add_list db rel l =
    List.iter
      (fun (x,y) -> DB.add_fact db (make rel x y))
      l

  let to_string t = name t ^ "/2"
  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end

module Rel3 = struct
  type ('a,'b,'c) t = Univ.t * 'a Univ.key * 'b Univ.key * 'c Univ.key

  let name (n, _, _, _) =
    match Univ.unpack ~key:Univ.string n with
    | None -> assert false
    | Some s -> s

  let create ?(k1=Univ.new_key ()) ?(k2=Univ.new_key ())
    ?(k3=Univ.new_key ()) name = (of_string name, k1, k2, k3)

  let get (name,k1,k2,k3) t = match t with
    | T.Apply (name',
      [| T.Apply(u1, [| |])
      ;  T.Apply(u2, [| |])
      ;  T.Apply(u3, [| |])
      |]) when Univ.eq name name' ->
      begin match Univ.unpack ~key:k1 u1, Univ.unpack ~key:k2 u2, Univ.unpack ~key:k3 u3 with
      | Some x1, Some x2, Some x3 -> Some (x1,x2,x3)
      | _ -> None
      end
    | _ -> None

  let make (n,k1,k2,k3) x1 x2 x3 =
    let a1 = T.mk_const (Univ.pack ~key:k1 x1) in
    let a2 = T.mk_const (Univ.pack ~key:k2 x2) in
    let a3 = T.mk_const (Univ.pack ~key:k3 x3) in
    T.mk_apply n [| a1; a2; a3 |]

  let apply (n,_,_,_) t1 t2 t3 = T.mk_apply n [| t1; t2; t3 |]

  (* find instances of the relation *)
  let find db ((n,_,_,_) as rel) =
    let query = T.mk_apply n [| T.mk_var 0; T.mk_var 1; T.mk_var 2 |] in
    let l = Logic.ask db query in
    List.fold_left
      (fun acc t -> match get rel t with
        | None -> acc
        | Some (x,y,z) -> (x,y,z) :: acc)
      [] l

  (* r1 => r2, so r1 subset of r2 *)
  let subset db (n1,_,_,_) (n2,_,_,_) =
    let x, y, z = T.mk_var 0, T.mk_var 1, T.mk_var 2 in
    let c = C.mk_clause
      (T.mk_apply n2 [|x;y;z|])
      [ Logic.Lit.mk_pos (T.mk_apply n1 [|x;y;z|]) ]
    in
    DB.add_clause db c

  let from_fun db ((n,_,_,_) as rel) f =
    DB.interpret db n
      (fun t -> match get rel t with
        | None -> []
        | Some (x,y,z) ->
          if f x y z
            then [C.mk_fact t]  (* the fact is true, says [f] *)
            else [])

  let add_list db rel l =
    List.iter
      (fun (x,y,z) -> DB.add_fact db (make rel x y z))
      l

  let to_string t = name t ^ "/3"
  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end

module RelList = struct
  type 'a t = (Univ.t * 'a Univ.key)

  let name (n, _) =
    match Univ.unpack ~key:Univ.string n with
    | None -> assert false
    | Some s -> s

  let create ?(k=Univ.new_key ()) name = of_string name, k

  let get (name, k) t = match t with
    | T.Apply (name', a) when Univ.eq name name' ->
      begin try
        let l = Array.fold_left
          (fun acc t' -> match t' with
            | T.Apply (u, [| |]) ->
              begin match Univ.unpack ~key:k u with
                | None -> raise Exit
                | Some x -> x :: acc
              end
            | _ -> raise Exit)
          [] a
        in
        Some (List.rev l)
      with Exit -> None
      end
    | _ -> None

  let make (name,k) l =
    let args = List.map (fun x -> T.mk_const (Univ.pack ~key:k x)) l in
    T.mk_apply_l name args
end

module Parse = struct
  include TopDown.MakeParse(struct
    type t = const
    let of_string = of_string
    let of_int = of_int
  end)(Logic)

  let _load db res =
    match res with
    | `Error _ -> false
    | `Ok clauses ->
      Logic.DB.add_clauses db clauses;
      true

  let load_chan db ic = _load db (parse_chan ic)

  let load_file db file = _load db (parse_file file)

  let load_string db s = _load db (parse_string s)
end

(** {2 Interpretation} *)

let add_builtin db =
  let builtin =
    let _eq goal = match goal with
      | Logic.T.Apply (_, [| Logic.T.Apply (a, [||]); Logic.T.Apply (b, [||]) |])
        when Univ.eq a b -> [ C.mk_fact goal ]
      | _ -> []
    and _neq goal = match goal with
      | Logic.T.Apply (_, [| Logic.T.Apply (a, [||]); Logic.T.Apply (b, [||]) |])
        when not (Univ.eq a b) -> [ Logic.C.mk_fact goal ]
      | _ -> []
    and _print goal =
      begin match goal with
      | Logic.T.Apply (_, [| a |]) when Logic.T.ground a ->
        Printf.printf "> %a\n" Logic.T.pp a;
      | _ -> ()
      end;
      [ Logic.C.mk_fact goal ]
    (* given a list of arguments, "replace" the goal by any of its arguments.
       this allow arguments (variables...) to get to the proposition level *)
    and _eval goal = match goal with
      | Logic.T.Apply (_, subgoals) ->
        (* for each goal \in subgoals, add a clause  goal :- subgoal *)
        Array.fold_left
          (fun acc sub -> Logic.C.mk_clause goal [Logic.Lit.mk_pos sub] :: acc)
          [] subgoals
      | _ -> []
    in
    [ of_string "=", "=; equality", _eq
    ; of_string "!=", "!=, inequality", _neq
    ; of_string "print", "print(a): print a term on stdout", _print
    ; of_string "eval",
        "eval(*goals): add eval(goals) :- g for each g in goals", _eval
    ]
  in
  DB.interpret_list db builtin;
  let _assertz db goal = match goal with
    | Logic.T.Apply(_, [| fact |]) when Logic.T.ground fact ->
      Logic.DB.add_fact db fact;
      [ C.mk_fact goal ]
    | _ -> []
  in
  DB.interpret ~help:"assertz(fact): add fact to the DB"
    db (of_string "assertz") (_assertz db);
  ()
