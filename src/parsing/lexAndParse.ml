open Core
open Lexer
open Lexing

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

module I = Parser.MenhirInterpreter

exception ParseError of string

let get_parse_error env =
  match I.stack env with
  | (lazy Nil) -> "Invalid syntax"
  | (lazy (Cons (I.Element (state, _, _, _), _))) -> (
    try ParserMessages.message (I.number state)
    with Not_found_s _ -> "invalid syntax (no specific message for this eror)" )

let rec parse lexbuf (checkpoint : ParsedAst.program I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError _env ->
      let err_msg =
        Fmt.str "%s: %s@." (print_error_position lexbuf) (get_parse_error _env) in
      raise (ParseError err_msg)
  | I.Accepted v -> v
  | I.Rejected ->
      let err_msg = "invalid syntax (parser rejected the input)" in
      raise (ParseError err_msg)

let parse_program lexbuf =
  (* Checking Lexer let rec getnext lex = let token = Lexer.token lex in
     PprintParserTokens.pprint_tokens Fmt.stdout token ; if token != Parser.EOF then
     getnext lex else 0 in getnext lexbuf ; *)
  try
    let grammar = parse lexbuf (Parser.Incremental.prog lexbuf.lex_curr_p) in
    Ok grammar
  with ParseError err_msg -> Error (Error.of_string err_msg)

let pprint_parsed_ast ppf (prog : ParsedAst.program) =
  PprintParsedAst.pprint_program ppf prog
