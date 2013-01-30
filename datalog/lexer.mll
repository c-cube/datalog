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
  open Utils

  let update_column_index (value: int) =
    prev_column_index := !current_column_index;
    current_column_index := value


  let update_line_index () =
    prev_line_index := !current_line_index;
    current_line_index := !current_line_index + 1;
    update_column_index 1


  let count_new_lines (string: string) : unit =
    String.iter
      (fun char ->
         if char = '\n' then
           update_line_index ()
         else
           update_column_index (!current_column_index + 1))
      string

  let update_token (token: string) =
    current_token := token;
    update_column_index (!current_column_index + String.length token)

  let lexing_error (error: string) (token: string) =
    Format.eprintf "%s %s, token '%s'@."
      error (Utils.print_location ()) token;
    raise PARSE_ERROR
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
      | [' ' '\t' '\r']              { update_token (Lexing.lexeme lexbuf);
                                       token lexbuf } (* skip blanks *)
      | ['\n']                       { update_line_index ();
                                       current_token := Lexing.lexeme lexbuf;
                                       token lexbuf } (* skip new lines *)
      | one_line_comment             { update_line_index ();
                                       current_token := Lexing.lexeme lexbuf;
                                       token lexbuf } (* skip comment *)
      | multi_line_comment           { count_new_lines (Lexing.lexeme lexbuf);
                                       current_token := Lexing.lexeme lexbuf;
                                       token lexbuf } (* skip comment *)
      | multi_line_comment_unclosed  { prev_column_index := !current_column_index;
                                       prev_line_index := !current_line_index;
                                       lexing_error "Unclosed Comment" (Lexing.lexeme lexbuf) }
          (* end of input - for channels, strings, ... *)
      | eof                          { update_token (Lexing.lexeme lexbuf); EOI }
      | lower_word                   { update_token (Lexing.lexeme lexbuf);
                                       LOWER_WORD(Lexing.lexeme lexbuf) }
      | upper_word                   { update_token (Lexing.lexeme lexbuf);
                                       UPPER_WORD(Lexing.lexeme lexbuf) }
      | single_quoted                { update_token (Lexing.lexeme lexbuf);
                                       SINGLE_QUOTED(Lexing.lexeme lexbuf) }
      | number                       { update_token (Lexing.lexeme lexbuf);
                                       INT(Lexing.lexeme lexbuf) }
      | '('                          { update_token (Lexing.lexeme lexbuf); LEFT_PARENTHESIS }
      | ')'                          { update_token (Lexing.lexeme lexbuf); RIGHT_PARENTHESIS }
      | '.'                          { update_token (Lexing.lexeme lexbuf); DOT }
      | ":-"                         { update_token (Lexing.lexeme lexbuf); IF }
      | ","                          { update_token (Lexing.lexeme lexbuf); COMMA }
      | _                            { prev_column_index := !current_column_index;
                                       prev_line_index := !current_line_index;
                                       lexing_error "Invalid Input" (Lexing.lexeme lexbuf) }

{

}
