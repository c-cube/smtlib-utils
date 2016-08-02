
(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 Utils around parsing} *)

val parse_file_exn : string -> Tip_ast.statement list

val parse_file : string -> (Tip_ast.statement list, string) Result.result

