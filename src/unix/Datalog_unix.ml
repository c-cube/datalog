
(* this file is part of datalog. See README for the license *)

(** {1 Unix Interpreted Predicates} *)

module TopDown = Datalog_top_down

module type S = sig
  module TD : TopDown.S

  val setup_handlers : TD.DB.t -> unit
end

(* main constructor *)

module Make(TD : TopDown.S) = struct
  module TD = TD

  module T = TD.T
  module C = TD.C

  let c2str = TD.Const.to_string
  let str2c = TD.Const.of_string

  let str_split ~by s =
    let rec next acc i j =
      if i = String.length s
        then if i > j
          then String.sub s j (i-j) :: acc
          else acc
      else if s.[i] = by
        then next (String.sub s j (i-j-1) :: acc) (i+1) (i+1)
      else next acc (i+1) j
    in
    next [] 0 0

  (* list files in directory *)
  let _ls = function
    | T.Apply (head, [| T.Apply (path, [| |]) as lit1; _ |] ) ->
      begin try
        let l = ref [] in
        let path = String.trim (c2str path) in
        let d = Unix.opendir path in
        let rec next () =
          let cont =
            try
              let s = Unix.readdir d in
              l := s :: !l;
              true
            with End_of_file -> false
          in if cont then next ()
        in
        next ();
        List.map
          (fun f ->
            let f = Filename.concat path f in
            let t = T.mk_apply head [| lit1; T.mk_const (str2c f) |] in
            C.mk_fact t)
          !l
      with _e -> []
      end
    | _ -> []

  let _stat lit = match lit with
    | T.Apply (head, [| T.Apply (path, [| |]) as t1; _ |]) ->
      begin try
        let path = String.trim (c2str path) in
        let stat = Unix.lstat path in
        let kind = match stat.Unix.st_kind with
          | Unix.S_LNK -> "link"
          | Unix.S_BLK -> "block"
          | Unix.S_DIR -> "dir"
          | Unix.S_FIFO -> "fifo"
          | Unix.S_REG -> "file"
          | Unix.S_SOCK -> "socket"
          | _ -> raise Exit
        in
        let kind = T.mk_const (str2c kind) in
        [ C.mk_fact (T.mk_apply head [| t1; kind |]) ]
      with Unix.Unix_error _ | Exit -> []
      end
    | _ -> []

  let _isDir lit = match lit with
    | T.Apply (_, [| T.Apply (path, [| |]) |]) ->
      begin try
        let path = String.trim (c2str path) in
        let stat = Unix.lstat path in
        let ok = match stat.Unix.st_kind with
          | Unix.S_DIR -> true
          | _ -> false
        in
        if ok
          then [ C.mk_fact lit ]
          else []
      with Unix.Unix_error _ -> []
      end
    | _ -> []

  let _isFile lit = match lit with
    | T.Apply (_, [| T.Apply (path, [| |]) |]) ->
      begin try
        let path = String.trim (c2str path) in
        let stat = Unix.lstat path in
        let ok = match stat.Unix.st_kind with
          | Unix.S_REG -> true
          | _ -> false
        in
        if ok
          then [ C.mk_fact lit ]
          else []
      with Unix.Unix_error _ -> []
      end
    | _ -> []

  let _isLink lit = match lit with
    | T.Apply (_, [| T.Apply (path, [| |]) |]) ->
      begin try
        let path = String.trim (c2str path) in
        let stat = Unix.lstat path in
        let ok = match stat.Unix.st_kind with
          | Unix.S_LNK -> true
          | _ -> false
        in
        if ok
          then [ C.mk_fact lit ]
          else []
      with Unix.Unix_error _ -> []
      end
    | _ -> []

  let _getenv lit = match lit with
    | T.Apply (head, [| T.Apply (var, [| |]) as t1; _ |]) ->
      begin try
        let v = str2c (Unix.getenv (c2str var)) in
        let t = T.mk_apply head [| t1; T.mk_const v |] in
        [ C.mk_fact t ]
      with Not_found -> []
      end
    | _ -> []

  let _split lit = match lit with
    | T.Apply (head, [| T.Apply (s, [| |]) as t1; T.Apply (by, [| |]) as t2; _ |]) ->
      let by = c2str by in
      let s = c2str s in
      if String.length by = 1
        then
          let l = str_split ~by:by.[0] s in
          List.map
            (fun tok ->
              C.mk_fact (T.mk_apply head [| t1; t2; T.mk_const (str2c tok) |]))
            l
        else []
    | _ -> []

  let _time = function
    | T.Apply (head, [| _ |]) ->
      let f = Unix.time () in
      let f = str2c (string_of_float f) in
      [ C.mk_fact (T.mk_apply head [| T.mk_const f |]) ]
    | _ -> []
      
  let handlers =
    [ str2c "ls", "ls(path, file): lists files in path", _ls
    ; str2c "stat", "stat(path,X): binds X to the kind of file path is", _stat
    ; str2c "isDir", "isDir(path): true if the path is a directory", _isDir
    ; str2c "isFile", "isFile(path): true if the path is a proper file", _isFile
    ; str2c "isLink", "isLink(path): true if the path is a symbolic link", _isLink
    ; str2c "getenv", "getenv(v,X): binds X to the environment variable v", _getenv
    ; str2c "split", "split(str,by,X): binds X to sustrings of str delimited by by", _split
    ; str2c "time", "time(T): binds T to the current time", _time
    ]

  let setup_handlers db = TD.DB.interpret_list db handlers
end

module Default = Make(TopDown.Default)
