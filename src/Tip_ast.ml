
(* This file is free software. See file "license" for more details. *)

(** {1 Trivial AST for parsing} *)

let pp_str = Format.pp_print_string

let pp_to_string pp x =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

module Loc = struct
  type t = {
    file : string;
    start_line : int;
    start_column : int;
    stop_line : int;
    stop_column : int;
  }

  let mk file start_line start_column stop_line stop_column =
    { file; start_line; start_column; stop_line; stop_column; }

  let mk_pair file (a,b)(c,d) = mk file a b c d

  let mk_pos start stop =
    let open Lexing in
    mk
      start.pos_fname
      start.pos_lnum (start.pos_cnum - start.pos_bol)
      stop.pos_lnum (stop.pos_cnum - stop.pos_bol)

  let equal = (=)

  let pp out pos =
    if pos.start_line = pos.stop_line
    then
      Format.fprintf out "file '%s': line %d, col %d to %d"
        pos.file pos.start_line pos.start_column pos.stop_column
    else
      Format.fprintf out "file '%s': line %d, col %d to line %d, col %d"
        pos.file
        pos.start_line pos.start_column
        pos.stop_line pos.stop_column

  let pp_opt out = function
    | None -> Format.fprintf out "<no location>"
    | Some pos -> pp out pos

  let to_string_opt = pp_to_string pp_opt

  (** {2 Lexbuf} *)

  let set_file buf filename =
    let open Lexing in
    buf.lex_curr_p <- {buf.lex_curr_p with pos_fname=filename;};
    ()

  let get_file buf =
    let open Lexing in
    buf.lex_curr_p.pos_fname

  let of_lexbuf lexbuf =
    let start = Lexing.lexeme_start_p lexbuf in
    let end_ = Lexing.lexeme_end_p lexbuf in
    let s_l = start.Lexing.pos_lnum in
    let s_c = start.Lexing.pos_cnum - start.Lexing.pos_bol in
    let e_l = end_.Lexing.pos_lnum in
    let e_c = end_.Lexing.pos_cnum - end_.Lexing.pos_bol in
    let file = get_file lexbuf in
    mk file s_l s_c e_l e_c
end

type var = string

type ty =
  | Ty_bool
  | Ty_const of string
  | Ty_arrow of ty list * ty

type typed_var = var * ty

(** {2 AST: S-expressions with locations} *)
type term =
  | True
  | False
  | Const of string
  | App of term * term list
  | Match of term * (string * var list * term) list
  | If of term * term * term
  | Fun of typed_var * term
  | Mu of typed_var * term
  | Eq of term * term
  | Imply of term * term
  | And of term list
  | Or of term list
  | Not of term

type statement = {
  stmt: stmt;
  loc: Loc.t option;
}

and stmt =
  | Stmt_include of string
  | Stmt_decl of string * ty
  | Stmt_def of (string * ty * term) list
  | Stmt_data of (string * (string * ty list) list) list
  | Stmt_assert of term
  | Stmt_goal of typed_var list * term (* satisfy this *)

let ty_bool = Ty_bool
let ty_const s = Ty_const s
let ty_arrow_l args ret = if args=[] then ret else Ty_arrow (args, ret)
let ty_arrow a b = ty_arrow_l [a] b

let true_ = True
let false_ = False
let const s = Const s
let app f l = match f, l with
  | _, [] -> f
  | App (f1, l1), _ -> App (f1, l1 @ l)
  | _ -> App (f, l)
let match_ u l = Match (u,l)
let if_ a b c = If(a,b,c)
let fun_ v t = Fun (v,t)
let fun_l = List.fold_right fun_
let eq a b = Eq (a,b)
let imply a b = Imply(a,b)
let and_ l = And l
let or_ l = Or l
let not_ t = Not t

let _mk ?loc stmt = { loc; stmt }

let include_ ?loc s = _mk ?loc (Stmt_include s)
let decl ?loc f ty = _mk ?loc (Stmt_decl (f, ty))
let def ?loc l = _mk ?loc (Stmt_def l)
let data ?loc l = _mk ?loc (Stmt_data l)
let assert_ ?loc t = _mk ?loc (Stmt_assert t)
let goal ?loc vars t = _mk ?loc (Stmt_goal (vars, t))

let loc t = t.loc
let view t = t.stmt

let fpf = Format.fprintf

let pp_list ?(start="") ?(stop="") ?(sep=" ") pp out l =
  let rec pp_list l = match l with
  | x::((_::_) as l) ->
    pp out x;
    Format.pp_print_string out sep;
    Format.pp_print_cut out ();
    pp_list l
  | x::[] -> pp out x
  | [] -> ()
  in
  Format.pp_print_string out start;
  pp_list l;
  Format.pp_print_string out stop


let rec pp_ty out (ty:ty) = match ty with
  | Ty_bool -> pp_str out "Bool"
  | Ty_const s -> pp_str out s
  | Ty_arrow (args,ret) ->
    fpf out "(@[->@ %a@ %a@])" (pp_list pp_ty) args pp_ty ret

let rec pp_term out (t:term) = match t with
  | True -> pp_str out "true"
  | False -> pp_str out "false"
  | Const s -> pp_str out s
  | App (f,l) -> fpf out "(@[<1>%a@ %a@])" pp_term f (pp_list pp_term) l
  | Match (lhs,cases) ->
    let pp_case out (c,vars,rhs) =
      if vars=[]
      then fpf out "(@[%s@ %a@])" c pp_term rhs
      else fpf out "(@[%s@ %a@ %a@])" c (pp_list pp_str) vars pp_term rhs
    in
    fpf out "(@[<1>match %a@ (@[<hv>%a@])@])" pp_term lhs
      (pp_list pp_case) cases
  | If (a,b,c) -> fpf out "(@[<hv1>if %a@ %a@ %a@])" pp_term a pp_term b pp_term c
  | Fun (v,body) -> fpf out "(@[<1>fun@ (%a)@ %a@])" pp_typed_var v pp_term body
  | Mu (v,body) -> fpf out "(@[<1>fun@ (%a)@ %a@])" pp_typed_var v pp_term body
  | Eq (a,b) -> fpf out "(@[=@ %a@ %a@])" pp_term a pp_term b
  | Imply (a,b) -> fpf out "(@[=>@ %a@ %a@])" pp_term a pp_term b
  | And l -> fpf out "(@[<hv>and@ %a@])" (pp_list pp_term) l
  | Or l -> fpf out "(@[<hv>or@ %a@])" (pp_list pp_term) l
  | Not t -> fpf out "(not %a)" pp_term t
and pp_typed_var out (v,ty) =
  fpf out "(@[%s@ %a@])" v pp_ty ty

let pp_stmt out (st:statement) = match view st with
  | Stmt_include s -> fpf out "(include %S)" s
  | Stmt_assert t -> fpf out "(@[assert@ %a@])" pp_term t
  | Stmt_goal (vars,t) ->
    fpf out "(@[goal@ (@[%a@])@ %a@])" (pp_list pp_typed_var) vars pp_term t
  | Stmt_decl (s, ty) ->
    fpf out "(@[decl@ %s@ %a@])" s pp_ty ty
  | Stmt_def l ->
    let pp_def out (s,ty,rhs) =
      fpf out "(@[<1>%s@ %a@ %a@])" s pp_ty ty pp_term rhs
    in
    fpf out "(@[<hv1>define@ %a@])" (pp_list pp_def) l
  | Stmt_data l ->
    let pp_cstor out (s,ty_args) =
      if ty_args=[] then pp_str out s
      else fpf out "(@[<1>%s@ %a@])" s (pp_list pp_ty) ty_args
    in
    let pp_data out (s,cstors) =
      fpf out "(@[<hv1>%s@ (@[<v>%a@]@])" s (pp_list pp_cstor) cstors
    in
    fpf out "(@[<hv>data@ @[<v>%a@]@])" (pp_list pp_data) l

(** {2 Errors} *)

exception Parse_error of Loc.t option * string

let () = Printexc.register_printer
    (function
      | Parse_error (loc, msg) ->
        let pp out () =
          Format.fprintf out "parse error at %a:@ %s" Loc.pp_opt loc msg
        in
        Some (pp_to_string pp ())
      | _ -> None)

let parse_error ?loc msg = raise (Parse_error (loc, msg))
let parse_errorf ?loc msg = Format.ksprintf (parse_error ?loc) msg
