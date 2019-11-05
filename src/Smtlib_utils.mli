(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 SMTLIB parser} *)

module Ast = Ast
module Loc = Loc
module Parser = Parser
module Lexer = Lexer

val parse_file_exn : string -> Ast.statement list

val parse_file : string -> (Ast.statement list, string) Result.result

val parse_chan_exn :
  ?filename:string ->
  in_channel ->
  Ast.statement list

val parse_chan :
  ?filename:string ->
  in_channel ->
  (Ast.statement list, string) Result.result

