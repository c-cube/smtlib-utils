
(** Simple parser/printer program *)

module Trace = Trace_core

module A = Smtlib_utils.V_2_6.Ast

let progress = ref false
let quiet = ref false
let (let@) = (@@)

type input =
  | Stdin
  | File of string

let string_of_input = function
  | Stdin -> "<stdin>"
  | File f -> Printf.sprintf "file `%s`" f

(* ANSI code for clearing current line *)
let _reset_line = "\x1b[2K\r"

let process i : (_,_) result =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__
"process-file" ~data:(fun () -> ["file", `String (string_of_input i)] ) in
  try
    let l = match i with
      | Stdin -> Smtlib_utils.V_2_6.parse_chan_exn stdin
      | File file ->
        if !progress then Printf.eprintf "%sprocess '%s'…%!" _reset_line file;
        let@ _sp = Trace.with_span  ~__FILE__ ~__LINE__ "parse"  in
        Smtlib_utils.V_2_6.parse_file_exn file
    in
    if not !quiet then (
      let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "pp" in
      Format.printf "; from %s@." (string_of_input i);
      Format.printf "@[<hv>%a@]@." (A.pp_list A.pp_stmt) l
    );
    if !progress then Printf.eprintf "%s%!" _reset_line;
    Ok ()
  with e ->
    let bt = Printexc.get_backtrace() in
    Error (Printexc.to_string e, bt)

let process_file f = process (File f)

let process_files l =
  Printf.printf "process %d files…\n%!" (List.length l);
  let errs = ref [] in
  List.iter
    (fun f -> match process_file f with
      | Ok () -> ()
      | Error (e,bt) -> errs := (f,e,bt):: !errs)
    l;
  if !errs=[] then Ok ()
  else Error !errs

let options =
  Arg.align [
    "--quiet", Arg.Set quiet, " quiet mode (check only)";
    "-q", Arg.Set quiet, " short for --quiet";
    "-p", Arg.Set progress, " print progress bar on stderr";
  ]

let () =
  let@ () = Trace_tef.with_setup ()in

  let l = ref [] in
  Arg.parse options (fun s -> l := s :: !l) "usage: tip-cat [file]*";

  let res =
    if !l=[]
    then
      process Stdin
      |> Result.map_error (fun (e,bt)->["<stdin>",e,bt])
    else process_files (List.rev !l)
  in
  match res with
  | Ok () -> ()
  | Error l ->
    Printf.eprintf "%d errors!\n" (List.length l);
    List.iter (fun (file,e,bt) ->
        Printf.eprintf "\n####\nerror on %S:\n%s:\n%s\n"
        file e bt)
      l;
    exit 1
