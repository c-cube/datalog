(* this file is part of datalog. See README for the license *)

{
open Parser

let print_location lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Format.sprintf "at line %d, column %d" pos.pos_lnum pos.pos_cnum

let lexing_error (error: string) lexbuf =
  let open Lexing in
  Format.eprintf "%s %s, token '%s'@." error (print_location lexbuf) (lexeme lexbuf);
  raise Parsing.Parse_error
}

let numeric = ['0' - '9']
let non_zero_numeric = ['1' - '9']
let number = ( '0' | ( non_zero_numeric numeric *) )
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = ( lower_alpha | upper_alpha | numeric | '_' )

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*
let single_quoted = "'" ([^ '''] | "\\'")+ "'"
let one_line_comment = '%' [^ '\n' '\r']* ('\n' | "\r\n")
let multi_line_comment = "/*" ( [^ '*'] | ('*' [^ '/']) )* "*/"
let multi_line_comment_unclosed = "/*" ( [^ '*'] | ('*' [^ '/']) )* eof

rule token =
    parse
      | [' ' '\t' '\r']              { token lexbuf } (* skip blanks *)
      | ['\n']                       { Lexing.new_line lexbuf;
                                       token lexbuf } (* skip new lines *)
      | "not"                        { NOT }
      | one_line_comment             { token lexbuf } (* skip comment *)
      | multi_line_comment           { token lexbuf } (* skip comment *)
      | multi_line_comment_unclosed  { lexing_error "Unclosed Comment" lexbuf }
          (* end of input - for channels, strings, ... *)
      | eof                          { EOI }
      | lower_word                   { LOWER_WORD(Lexing.lexeme lexbuf) }
      | upper_word                   { UPPER_WORD(Lexing.lexeme lexbuf) }
      | single_quoted                { SINGLE_QUOTED(Lexing.lexeme lexbuf) }
      | number                       { INT(Lexing.lexeme lexbuf) }
      | '('                          { LEFT_PARENTHESIS }
      | ')'                          { RIGHT_PARENTHESIS }
      | '.'                          { DOT }
      | ":-"                         { IF }
      | "<-"                         { IF }
      | ","                          { COMMA }
      | _                            { lexing_error "Invalid Input" lexbuf }

{
  let print_location = print_location

}
