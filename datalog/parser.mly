
%{

  (** Mapping string -> var in the current context *)
  let vars = Hashtbl.create 3
  let vars_num = ref (-1)

  (** Reset the variables mapping *)
  let reset_vars () =
    Hashtbl.clear vars;
    vars_num := (-1)

  (** Get the number for this variable, in current variables context *)
  let get_var name =
    try Hashtbl.find vars name
    with Not_found ->
      let i = !vars_num in
      decr vars_num;
      Hashtbl.replace vars name (`Var i);
      `Var i

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

%start term
%type <Logic.Default.term> term

%start rule
%type <Logic.Default.rule> rule

%start parse_file
%type <Logic.Default.rule list> parse_file

%%

parse_file:
  | rules EOI { $1 }

rules: 
  | rule { let r = [$1] in reset_vars (); r }
  | rule rules { $1 :: $2 }

rule:
  | term DOT { Logic.Default.mk_rule $1 [] }
  | term IF terms DOT { Logic.Default.mk_rule $1 $3 }

terms:
  | term { [$1] }
  | term COMMA terms { $1 :: $3 }

term:
  | LOWER_WORD { Logic.Default.mk_term $1 [] }
  | LOWER_WORD LEFT_PARENTHESIS args RIGHT_PARENTHESIS
    { Logic.Default.mk_term $1 $3 }

args:
  | const { [`Symbol $1] }
  | UPPER_WORD { [get_var $1] }
  | const COMMA args { (`Symbol $1) :: $3 } 
  | UPPER_WORD COMMA args { (get_var $1) :: $3 }

const:
  | INT { $1 }
  | LOWER_WORD { $1 }
  | SINGLE_QUOTED { $1 }
