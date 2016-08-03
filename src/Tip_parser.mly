

(* This file is free software, part of nunchaku. See file "license" for more details. *)

(** {1 Parser for Nunchaku} *)

(* vim:SyntasticToggleMode: *)

%{
  module A = Tip_ast
  module Loc = Tip_loc

%}

%token EOI

%token LEFT_PAREN
%token RIGHT_PAREN

%token BOOL
%token PAR
%token ARROW

%token TRUE
%token FALSE
%token OR
%token AND
%token NOT
%token EQ
%token IF
%token MATCH
%token CASE
%token FUN
%token LET
%token AS

%token DATA
%token ASSERT
%token ASSERT_NOT
%token FORALL
%token DECL
%token DECLARE_SORT
%token DEFINE_FUN_REC
%token DEFINE_FUNS_REC
%token CHECK_SAT

%token <string>IDENT
%token <string>QUOTED

%start <Tip_ast.statement> parse
%start <Tip_ast.statement list> parse_list

%%

parse_list: l=stmt* EOI {l}

parse: t=stmt EOI { t }

cstor_arg:
  | LEFT_PAREN name=IDENT ty=ty RIGHT_PAREN { name, ty }

cstor:
  | LEFT_PAREN c=IDENT RIGHT_PAREN { A.mk_cstor c [] }
  | LEFT_PAREN c=IDENT l=cstor_arg+ RIGHT_PAREN
    { A.mk_cstor c l }

data:
  | LEFT_PAREN s=IDENT l=cstor+ RIGHT_PAREN { s,l }

fun_decl_mono:
  | f=IDENT
    LEFT_PAREN args=typed_var* RIGHT_PAREN
    ret=ty
    { f, args, ret }

fun_rec:
  | tup=fun_decl_mono body=term
    {
      let f, args, ret = tup in
      A.mk_fun_rec ~ty_vars:[] f args ret body
    }
  | LEFT_PAREN
      PAR
      LEFT_PAREN l=tyvar* RIGHT_PAREN
      LEFT_PAREN tup=fun_decl_mono body=term RIGHT_PAREN
    RIGHT_PAREN
    {
      let f, args, ret = tup in
      A.mk_fun_rec ~ty_vars:l f args ret body
    }

funs_rec_decl:
  | LEFT_PAREN tup=fun_decl_mono RIGHT_PAREN
    {
      let f, args, ret = tup in
      A.mk_fun_decl ~ty_vars:[] f args ret
    }
  | LEFT_PAREN
      PAR
      LEFT_PAREN l=tyvar* RIGHT_PAREN
      LEFT_PAREN tup=fun_decl_mono RIGHT_PAREN
    RIGHT_PAREN
    {
      let f, args, ret = tup in
      A.mk_fun_decl ~ty_vars:l f args ret
    }

assert_not_forall:
  | LEFT_PAREN FORALL LEFT_PAREN vars=typed_var+ RIGHT_PAREN
    f=term
    RIGHT_PAREN
    { vars, f }
  | f=term { [], f}

assert_not:
  | LEFT_PAREN
      PAR LEFT_PAREN tyvars=tyvar+ RIGHT_PAREN tup=assert_not_forall
    RIGHT_PAREN
  { let vars, t = tup in  tyvars, vars, t }
  | tup=assert_not_forall
  { let vars, t = tup in  [], vars, t }

stmt:
  | LEFT_PAREN ASSERT t=term RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.assert_ ~loc t
    }
  | LEFT_PAREN DECLARE_SORT s=IDENT n=IDENT RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      try
        let n = int_of_string n in
        A.decl_sort ~loc s ~arity:n
      with Failure _ ->
        A.parse_errorf ~loc "expected arity to be an integer, not `%s`" n
    }
  | LEFT_PAREN DECL s=IDENT ty=ty RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.decl ~loc s ty
    }
  | LEFT_PAREN DATA
      LEFT_PAREN vars=tyvar* RIGHT_PAREN
      LEFT_PAREN l=data+ RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.data ~loc vars l
    }
  | LEFT_PAREN
    DEFINE_FUN_REC
    f=fun_rec
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.fun_rec ~loc f
    }
  | LEFT_PAREN
    DEFINE_FUNS_REC
      LEFT_PAREN decls=funs_rec_decl+ RIGHT_PAREN
      LEFT_PAREN bodies=term+ RIGHT_PAREN
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.funs_rec ~loc decls bodies
    }
  | LEFT_PAREN
    ASSERT_NOT
    tup=assert_not
    RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      let ty_vars, vars, f = tup in
      A.assert_not ~loc ~ty_vars vars f
    }
  | LEFT_PAREN CHECK_SAT RIGHT_PAREN
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.check_sat ~loc ()
    }
  | error
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.parse_errorf ~loc "expected statement"
    }

var:
  | s=IDENT { s }
tyvar:
  | s=IDENT { s }

ty:
  | BOOL { A.ty_bool }
  | s=IDENT { A.ty_const s }
  | LEFT_PAREN s=IDENT args=ty+ RIGHT_PAREN
    { A.ty_app s args }
  | LEFT_PAREN ARROW arg=ty ret=ty RIGHT_PAREN
    { A.ty_arrow arg ret }

typed_var:
  | LEFT_PAREN s=IDENT ty=ty RIGHT_PAREN { s, ty }

case:
  | LEFT_PAREN
      CASE
      c=IDENT
      rhs=term
    RIGHT_PAREN
    { c, [], rhs }
  | LEFT_PAREN
      CASE
      LEFT_PAREN c=IDENT vars=var+ RIGHT_PAREN
      rhs=term
    RIGHT_PAREN
    { c, vars, rhs }

binding:
  | LEFT_PAREN v=var t=term RIGHT_PAREN { v, t }

term:
  | TRUE { A.true_ }
  | FALSE { A.false_ }
  | s=QUOTED { A.const s }
  | s=IDENT { A.const s }
  | LEFT_PAREN IF a=term b=term c=term RIGHT_PAREN { A.if_ a b c }
  | LEFT_PAREN OR l=term+ RIGHT_PAREN { A.or_ l }
  | LEFT_PAREN AND l=term+ RIGHT_PAREN { A.and_ l }
  | LEFT_PAREN NOT t=term RIGHT_PAREN { A.not_ t }
  | LEFT_PAREN EQ a=term b=term RIGHT_PAREN { A.eq a b }
  | LEFT_PAREN ARROW a=term b=term RIGHT_PAREN { A.imply a b }
  | LEFT_PAREN f=term args=term+ RIGHT_PAREN { A.app f args }
  | LEFT_PAREN
      MATCH
      lhs=term
      l=case+
    RIGHT_PAREN
    { A.match_ lhs l }
  | LEFT_PAREN
      FUN
      LEFT_PAREN vars=typed_var+ RIGHT_PAREN
      body=term
    RIGHT_PAREN
    { A.fun_l vars body }
  | LEFT_PAREN
      LET
      LEFT_PAREN l=binding+ RIGHT_PAREN
      r=term
    RIGHT_PAREN
    { A.let_ l r }
  | LEFT_PAREN AS t=term ty=ty RIGHT_PAREN
    { A.cast t ~ty }
  | error
    {
      let loc = Loc.mk_pos $startpos $endpos in
      A.parse_errorf ~loc "expected term"
    }

%%
