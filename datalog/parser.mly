/*
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
*/

%{

  (** Mapping string -> var in the current context *)
  let vars = Hashtbl.create 3
  let vars_num = ref 0

  (** Reset the variables mapping *)
  let reset_vars () =
    Hashtbl.clear vars;
    vars_num := 0

  (** Get the number for this variable, in current variables context *)
  let get_var name =
    try Hashtbl.find vars name
    with Not_found ->
      let i = !vars_num in
      incr vars_num;
      Hashtbl.replace vars name i;
      i

%}

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token IF
%token COMMA
%token EOI
%token <string> SINGLE_QUOTED
%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> INT

%start parse_literal
%type <Logic.Default.literal> parse_literal

%start parse_clause
%type <Logic.Default.clause> parse_clause

%start parse_file
%type <Logic.Default.clause list> parse_file

%%

parse_file:
  | clauses EOI { reset_vars (); Utils.reset (); $1 }

parse_literal:
  | literal EOI { reset_vars (); Utils.reset (); $1 }

parse_clause:
  | clause EOI { reset_vars (); Utils.reset (); $1 }

clauses:
  | clause { let r = [$1] in reset_vars (); r }
  | clause clauses { $1 :: $2 }

clause:
  | literal DOT { Logic.Default.mk_clause $1 [] }
  | literal IF literals DOT { Logic.Default.mk_clause $1 $3 }

literals:
  | literal { [$1] }
  | literal COMMA literals { $1 :: $3 }

literal:
  | UPPER_WORD { Logic.Default.T.mk_var (get_var $1) }
  | const { Logic.Default.T.mk_const $1 }
  | const LEFT_PARENTHESIS args RIGHT_PARENTHESIS
    { Logic.Default.T.mk_apply_l (Logic.Default.T.mk_const $1) $3 }

args:
  | literal { [$1] }
  | literal COMMA args { $1 :: $3 }

const:
  | INT { $1 }
  | LOWER_WORD { $1 }
  | SINGLE_QUOTED { $1 }
