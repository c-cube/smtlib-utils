
(* This file is free software, copyright Simon Cruanes. See file "LICENSE" for more details. *)

(** {1 Simple parser/printer} *)

module A = Tip_ast

let process_file file =
  let l = Tip_util.parse_file_exn file in
  Format.printf "; file `%s`@." file;
  Format.printf "@[<hv>%a@]@." (A.pp_list A.pp_stmt) l

let options =
  Arg.align [ ]

let () =
  let l = ref [] in
  Arg.parse options (fun s -> l := s :: !l) "usage: tip-cat [file]*";
  try
    List.iter process_file (List.rev !l)
  with e ->
    Printexc.print_backtrace stdout;
    print_endline (Printexc.to_string e);
    exit 1
