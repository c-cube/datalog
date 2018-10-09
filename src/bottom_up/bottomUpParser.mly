/*
this file is part of datalog. See README for the license
*/

%{
%}

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token IF
%token NOT
%token COMMA
%token EOI
%token <string> SINGLE_QUOTED
%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> INT

%start parse_literal
%type <BottomUpAst.literal> parse_literal

%start parse_literals
%type <BottomUpAst.literal list> parse_literals

%start parse_clause
%type <BottomUpAst.clause> parse_clause

%start parse_file
%type <BottomUpAst.file> parse_file

%start parse_query
%type <BottomUpAst.query> parse_query

%%

parse_file:
  | clauses EOI { $1 }

parse_literal:
  | literal EOI { $1 }

parse_literals:
  | literals EOI { $1 }

parse_clause:
  | clause EOI { $1 }

parse_query:
  | query EOI { $1 }

clauses:
  | clause { [$1] }
  | clause clauses { $1 :: $2 }

clause:
  | literal DOT { BottomUpAst.Clause ($1, []) }
  | literal IF literals DOT { BottomUpAst.Clause ($1, $3) }

literals:
  | literal { [$1] }
  | literal COMMA literals { $1 :: $3 }

literal:
  | LOWER_WORD { BottomUpAst.Atom ($1, []) }
  | LOWER_WORD LEFT_PARENTHESIS args RIGHT_PARENTHESIS
    { BottomUpAst.Atom ($1, $3) }

query:
  | LEFT_PARENTHESIS args RIGHT_PARENTHESIS IF signed_literals
    {
      let pos_literals, neg_literal = $5 in
      BottomUpAst.Query ($2, pos_literals, neg_literal)
    }

signed_literals:
  | literal COMMA signed_literals
    { let pos, neg = $3 in $1 :: pos, neg }
  | NOT literal COMMA signed_literals
    { let pos, neg = $4 in pos, $2 :: neg }
  | literal { [$1], [] }
  | NOT literal { [], [$2] }

args:
  | term { [$1] }
  | term COMMA args  { $1 :: $3 }

term:
  | INT { BottomUpAst.Const $1 }
  | LOWER_WORD { BottomUpAst.Const $1 }
  | UPPER_WORD { BottomUpAst.Var $1 }
  | SINGLE_QUOTED { BottomUpAst.Quoted $1 }
