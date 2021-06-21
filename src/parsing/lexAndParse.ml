open Core
open Lexer
open Lexing

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  (* let rec getnext lex = let token = Lexer.token lex in PprintParserTokens.pprint_tokens
     Fmt.stdout token; if token != Parser.EOF then getnext lex else 0 in getnext lexbuf; *)

  (* let rec getnext = let token = Lexer.token lexbuf in PprintParserTokens.pprint_tokens
     Fmt.stdout token; if token != Parser.eof then getnext else 0 in getnext; "" *)

  (* while true tokens = Lexer.token lexbuf in (*List.iter
     ~f:(PprintParserTokens.pprint_tokens Fmt.stdout) tokens; *)
     PprintParserTokens.pprint_tokens Fmt.stdout tokens; *)
  try Ok (Parser.prog Lexer.token lexbuf) with
  (* Unfortunately the lexer and parser throw exceptions - so here we swallow the exn into
     the Result monad*)
  | Lexical_error ->
      let error_msg = Fmt.str "%s: lexical error@." (print_error_position lexbuf) in
      Error (Error.of_string error_msg)
  | SyntaxError msg ->
      let error_msg = Fmt.str "%s: %s@." (print_error_position lexbuf) msg in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg = Fmt.str "%s: syntax error@." (print_error_position lexbuf) in
      Error (Error.of_string error_msg)

let pprint_parsed_ast ppf (prog : ParsedAst.program) =
  PprintParsedAst.pprint_program ppf prog
