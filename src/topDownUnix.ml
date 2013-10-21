
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

(** {1 Unix Interpreted Predicates} *)


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

  (* TODO: inline documentation, to be used for documenting which
    predicates are intepreted and how *)

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
            let t = T.mk_apply head [| lit1; T.mk_const (str2c f) |] in
            C.mk_fact t)
          !l
      with e -> []
      end
    | _ -> []
      
  let handlers =
    [ str2c "ls", _ls
    ]

  let setup_handlers db = TD.DB.interpret_list db handlers
end

module Default = Make(TopDown.Default)
