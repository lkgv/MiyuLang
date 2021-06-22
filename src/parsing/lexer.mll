{
  
  open Parser
  open Lexing
  open Utils

  exception SyntaxError of string
  exception Lexical_error

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
      }

  let escaped_characters = [
    ("\"", "\"");
    ("\\", "\\");
    ("\'", "'");
    ("n", "\n");
    ("t", "\t");
    ("b", "\b");
    ("r", "\r");
    (" ", " ");
  ]

  let reserved = Assoc.of_list [
    ("import",    IMPORT);
    ("module",    MODULE);
    ("break",     BREAK);
    ("continue",  CONTINUE);
    ("fn",        FN);
    ("if",        IF);
    ("elif",      ELIF);
    ("else",      ELSE);
    ("const",     CONST);
    ("pub",       PUB);
    ("begin",     BEGIN);
    ("end",       END);
    ("loop",      LOOP);
    ("while",     WHILE);
    ("for",       FOR);
    ("in",        IN);
    ("let",       LET);
    ("ret",       RET);
    ("true",      BOOL true);
    ("false",     BOOL false);
    ("cls",       CLASS);
    ("trait",     TRAIT);
    ("impl",      IMPL);
    ("print",     PRINT);
    ("ref",       REF);
    ("new",       NEW);
    ("del",       DEL);
    ("this",      THIS);
    ("null",      NULL);
    ("static",    STATIC);
  ]
}

let newline = '\r' | '\n' | "\r\n"

let lname = ( '_'
            | ['a'-'z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
            | ['_'] ['a'-'z' 'A'-'Z' '0'-'9']+ ['_' 'a'-'z' 'A'-'Z' '0'-'9']*)

let uname = ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

let gname = '\'' ['a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

let hexdig = ['0'-'9' 'a'-'f' 'A'-'F']

let int = ['0'-'9'] ['0'-'9' '_']*

let xxxint =
    ( ("0x" | "0X") hexdig (hexdig | '_')*
    | ("0o" | "0O") ['0'-'7'] ['0'-'7' '_']*
    | ("0b" | "0B") ['0' '1'] ['0' '1' '_']*)

let float =
  '-'? ['0'-'9'] ['0'-'9' '_']*
  (('.' ['0'-'9' '_']*) (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)? |
   ('.' ['0'-'9' '_']*)? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*))

rule token = parse
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | ':'                 { COLON }
  | "::"                { COLONCOLON }
  | ','                 { COMMA }
  | '.'                 { DOT }
  | ';'                 { SEMI }
  | "->"                { ARROW }
  | '='                 { ASSIGN }
  | "=="                { EQUAL }
  | '>'                 { LARGER }
  | ">="                { LEQ }
  | '<'                 { SMALLER }
  | "<="                { SEQ }
  | "!="                { NEQ }
  | '*'                 { STAR }
  | "*="                { MUL_ASSIGN }
  | '/'                 { DIVIDE }
  | "/="                { DIV_ASSIGN }
  | '%'                 { MOD }
  | "%="                { MOD_ASSIGN }
  | '+'                 { PLUS }
  | "+="                { PLUS_ASSIGN }
  | '-'                 { MINUS }
  | "-="                { MINUS_ASSIGN }
  | '@'                 { AT }
  | '!'                 { NOT }
  | '|'                 { BAR }
  | "||"                { BARBAR }
  | '&'                 { AMPER }
  | "&&"                { AMPERAMPER }
  | newline             { Lexing.new_line lexbuf; token lexbuf }
  | "/*"                { comment 0 lexbuf }
  | "//"                 { single_line_comment lexbuf }
  | [' ' '\r' '\t']     { token lexbuf }
  | float               { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '"'                 { STRING (string "" lexbuf) }
  | lname               { let s = Lexing.lexeme lexbuf in
                            match Assoc.lookup s reserved with
                              | Some t -> t
                              | None -> LNAME s
                        }
  | uname               { UNAME ( Lexing.lexeme lexbuf ) }
  | gname               { GNAME ( Lexing.lexeme lexbuf ) }
  | int                 { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | xxxint              { try
                            INT (int_of_string (Lexing.lexeme lexbuf))
                          with Failure _ -> raise Lexical_error
                        }
  | eof                 { EOF }
  | _                   { raise Lexical_error }


and comment n = parse
  | "*/"                { if n = 0 then token lexbuf else comment (n - 1) lexbuf }
  | "/*"                { comment (n + 1) lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comment n lexbuf }
  | _                   { comment n lexbuf }
  | eof                 { raise Lexical_error }

and single_line_comment = parse
  | '\n'                { next_line lexbuf; token lexbuf }
  | eof                 { EOF }
  | _                   { single_line_comment lexbuf }

and string acc = parse
  | '"'                 { acc }
  | '\\'                { let esc = escaped lexbuf in string (acc ^ esc) lexbuf }
  | [^'"' '\\']*        { string (acc ^ (Lexing.lexeme lexbuf)) lexbuf }
  | eof                 { raise Lexical_error }

and escaped = parse
  | _                   { let str = Lexing.lexeme lexbuf in
                          try List.assoc str escaped_characters
                          with Not_found -> raise Lexical_error
                        }
