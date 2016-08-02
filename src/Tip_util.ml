
(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 Utils around parsing} *)

module A = Tip_ast

let with_in_ file f =
  let ic = open_in file in
  try
    let x = f ic in
    close_in ic;
    x
  with e ->
    close_in_noerr ic;
    raise e

let parse_file_exn file : A.statement list =
  with_in_ file
    (fun ic ->
       let lexbuf = Lexing.from_channel ic in
       A.Loc.set_file lexbuf file;
       Tip_parser.parse_list Tip_lexer.token lexbuf)

let parse_file file =
  try Result.Ok (parse_file_exn file)
  with e -> Result.Error (Printexc.to_string e)
