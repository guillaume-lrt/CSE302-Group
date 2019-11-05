{
  open Parser

  exception Error of string
}

let number = '-'? ['0'-'9'] +

(* Initial characters for variables *)
let ichar = ['A'-'Z' 'a'-'z' '_']

(* Characters allowed only in the body of variables. *)
let bchar = ['0'-'9']

let ident = ichar (ichar|bchar)*
let blank = ' ' | '\t' | '\r'

rule token = parse
| eof
    { ENDOFSTREAM }
| blank
    { token lexbuf }
| '\n'
    { Lexing.new_line lexbuf ; token lexbuf }
| "print"
    { PRINT }
| "if"
    { IF }
| "else"
    { ELSE }
| "while"
    { WHILE }
| "var"
    { VAR }
| "true"
    { BOOL true }
| "false"
    { BOOL false }
| "int64"
    { TY_INT64 }
| "bool"
    { TY_BOOL }
| "fun"
    { FUN }
| "proc"
    { PROC }
| "return"
    { RETURN }
| ident as x
    { IDENT x }
| number as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { ASTERISK }
| '/'
    { SLASH }
| '%'
    { PERCENT }
| "&&"
    { AMPAMP }
| '&'
    { AMP }
| "||"
    { BARBAR }
| '|'
    { BAR }
| '!'
    { BANG }
| '^'
    { CARET }
| "<<"
    { LTLT }
| ">>"
    { GTGT }
| '~'
    { TILDE }
| "=="
    { EQEQ }
| "!="
    { BANGEQ }
| "<="
    { LTEQ }
| '<'
    { LT }
| ">="
    { GTEQ }
| '>'
    { GT }
| '='
    { EQ }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| ';'
    { SEMICOLON }
| ':'
    { COLON }
| ','
    { COMMA }
| "//"
    { linecom lexbuf }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and linecom = parse
| eof
    { ENDOFSTREAM }
| '\n'
    { Lexing.new_line lexbuf ; token lexbuf }
| _
    { linecom lexbuf }