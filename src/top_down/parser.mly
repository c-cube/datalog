/*
this file is part of datalog. See README for the license
*/

%{
  let remove_quotes s =
    let n = String.length s in
    if (s.[0] = '\'' && s.[n-1] = '\'') ||
       (s.[0] = '"' && s.[n-1] = '"')
      then String.sub s 1 (n-2)
      else s
%}

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token COLON
%token IF
%token NOT
%token COMMA
%token AGGR_EQUAL
%token EOI
%token <string> SINGLE_QUOTED
%token <string> DOUBLE_QUOTED
%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> INT
%token <string> OPERATOR  /* infix operator */

%start parse_term
%type <AST.term> parse_term

%start parse_literal
%type <AST.literal> parse_literal

%start parse_literals
%type <AST.literal list> parse_literals

%start parse_query
%type <AST.term list * AST.literal list> parse_query

%start parse_clause
%type <AST.clause> parse_clause

%start parse_file
%type <AST.file> parse_file

%%

parse_file:
  | clauses EOI { $1 }

parse_term:
  | term EOI { $1 }

parse_literal:
  | literal EOI { $1 }

parse_literals:
  | literals EOI { $1 }

parse_query:
  | tuple IF literals { $1, $3 }

parse_clause:
  | clause EOI { $1 }

clauses:
  | clause { [$1] }
  | clause clauses { $1 :: $2 }

clause:
  | term DOT { ($1, []) }
  | term IF literals DOT { ($1, $3) }

literals:
  | literal { [$1] }
  | literal COMMA literals { $1 :: $3 }

literal:
  | atom { AST.LitPos $1 }
  | NOT atom { AST.LitNeg $2 }
  | subterm AGGR_EQUAL LOWER_WORD UPPER_WORD COLON term
    { AST.(LitAggr
      { ag_left=$1; ag_constructor= $3; ag_var= $4; ag_guard= $6}
      )
    }

atom:
  | term { $1 }
  | subterm OPERATOR subterm { AST.Apply($2, [$1; $3]) }

term:
  | LOWER_WORD { AST.Apply ($1, []) }
  | SINGLE_QUOTED { AST.Apply (remove_quotes $1, []) }
  | DOUBLE_QUOTED { AST.Apply (remove_quotes $1, []) }
  | LOWER_WORD LEFT_PARENTHESIS args RIGHT_PARENTHESIS
    { AST.Apply ($1, $3) }

subterm:
  | term { $1 }
  | UPPER_WORD { AST.Var $1 }
  | INT { AST.Int( int_of_string $1) }

args:
  | subterm { [$1] }
  | subterm COMMA args  { $1 :: $3 }

tuple:
  | LEFT_PARENTHESIS args RIGHT_PARENTHESIS { $2 }
