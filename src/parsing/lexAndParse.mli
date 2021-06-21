(** This module executes the lexer and parser. It acts as an interface between the parsing
    code and the main method (abstracting away the underlying implementation) *)

open Core

val parse_program : Lexing.lexbuf -> ParsedAst.program Or_error.t
(** Given a lex buffer to read a bolt program from, parse the program and return the AST
    if successful*)

val pprint_parsed_ast : Format.formatter -> ParsedAst.program -> unit
(** Given a formatter and parsed AST, pretty-print the AST - useful for debugging *)
