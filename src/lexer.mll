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

{
  open Parser 
  
  let fail () = raise Parsing.Parse_error

  let print_location lexbuf =
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Format.sprintf "at line %d, column %d" pos.pos_lnum pos.pos_cnum
}

let numeric = ['0' - '9']
let non_zero_numeric = ['1' - '9']
let number = ( '0' | ( non_zero_numeric numeric *) )
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = ( lower_alpha | upper_alpha | numeric | '_' )
let operator = ['_' '+' '<' '>' '=' '*' '-' '%' '^' '@' '|' ':' '!' '/']+

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*
let single_quoted = "'" ([^ '''] | "\\'")+ "'"
let double_quoted = "\"" ([^ '''] | "\\'")+ "\""
let one_line_comment = '%' [^ '\n' '\r']* ('\n' | "\r\n")
let multi_line_comment = "/*" ( [^ '*'] | ('*' [^ '/']) )* "*/"
let multi_line_comment_unclosed = "/*" ( [^ '*'] | ('*' [^ '/']) )* eof

rule token =
    parse
      | [' ' '\t' '\r']              { token lexbuf } (* skip blanks *)
      | ['\n']                       { Lexing.new_line lexbuf;
                                       token lexbuf } (* skip new lines *)
      | one_line_comment             { Lexing.new_line lexbuf;
                                       token lexbuf } (* skip comment *)
      | multi_line_comment           { token lexbuf } (* skip comment TODO new_line *)
      | multi_line_comment_unclosed  { fail () }
          (* end of input - for channels, strings, ... *)
      | eof                          { EOI }
      | "~"                          { NOT }
      | '('                          { LEFT_PARENTHESIS }
      | ')'                          { RIGHT_PARENTHESIS }
      | '.'                          { DOT }
      | ":"                          { COLON }
      | ":="                         { AGGR_EQUAL }
      | ":-"                         { IF }
      | "<-"                         { IF }
      | ","                          { COMMA }
      | lower_word                   { LOWER_WORD(Lexing.lexeme lexbuf) }
      | upper_word                   { UPPER_WORD(Lexing.lexeme lexbuf) }
      | single_quoted                { SINGLE_QUOTED(Lexing.lexeme lexbuf) }
      | double_quoted                { DOUBLE_QUOTED(Lexing.lexeme lexbuf) }
      | number                       { INT(Lexing.lexeme lexbuf) }
      | operator                     { OPERATOR(Lexing.lexeme lexbuf) }
      | _                            { fail ()}

{
  let print_location = print_location
}
