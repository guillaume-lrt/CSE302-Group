(* The abstract syntax trees *)

open Utils

(** Source syntax *)

type ty = Int64 | Bool

let pp_ty ff ty =
  let open Format in
  match ty with
  | Int64 -> pp_print_string ff "int64"
  | Bool -> pp_print_string ff "bool"

type var = Var of string [@@unboxed]

let pp_var ff (Var v) =
  Format.(pp_print_string ff v)

(** Binary operators *)
type binop =
  | Add | Subtract | Multiply | Divide | Modulus
  | BitAnd | BitOr | BitXor | Lshift | Rshift
  | Lt | Leq | Gt | Geq | Eq | Neq
  | LogAnd | LogOr

let pp_binop ff op =
  Format.pp_print_string ff begin
    match op with
    | Add -> "+" | Subtract -> "-"
    | Multiply -> "*" | Divide -> "/" | Modulus -> "%"
    | BitAnd -> "&" | BitOr -> "|" | BitXor -> "^"
    | Lshift -> "<<" | Rshift -> ">>"
    | Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">="
    | Eq -> "==" | Neq -> "!="
    | LogAnd -> "&&" | LogOr -> "||"
  end

let prec_binop = function
  | LogOr -> 3 | LogAnd -> 6
  | BitOr -> 10 | BitXor -> 20 | BitAnd -> 30
  | ( Eq | Neq ) -> 33
  | ( Lt | Leq | Gt | Geq ) -> 36
  | ( Lshift | Rshift ) -> 40
  | ( Add | Subtract ) -> 50
  | ( Multiply | Divide | Modulus ) -> 60

(** Unary operators *)
type unop =
  | Negate
  | BitNot
  | LogNot

let pp_unop ff op =
  Format.pp_print_string ff begin
    match op with
    | Negate -> "-"
    | BitNot -> "~"
    | LogNot -> "!"
  end

type pos = Lexing.position

(** Expressions *)
type expr_ =
  | Number of int
  (** Literal int *)
  | Boolean of bool
  (** Literal bool *)
  | Read of var
  (** Load a value *)
  | Unop of unop * expr
  (** Unary operator *)
  | Binop of binop * expr * expr
  (** Binary operator *)
  | Call of string * expr list
  (** Fun/Proc call *)

and expr = { expr : expr_ ; pos : Lexing.position ; mutable ty : ty option }

let prec_expr = function
  | Number _ | Boolean _ | Read _ | Call _ -> max_int
  | Unop _ -> 9999
  | Binop (bop, _, _) -> prec_binop bop

let rec pp_expr ff e =
  let open Format in
  let bracket ee =
    if prec_expr e.expr > prec_expr ee.expr then
      fprintf ff "(%a)" pp_expr ee
    else pp_expr ff ee in
  match e.expr with
  | Number n -> pp_print_int ff n
  | Boolean b -> pp_print_bool ff b
  | Read v -> pp_var ff v
  | Unop (op, e) ->
      pp_open_box ff 2 ; begin
        pp_unop ff op ;
        pp_print_space ff () ;
        bracket e ;
      end ; pp_close_box ff ()
  | Binop (op, l, r) ->
      pp_open_box ff 2 ; begin
        bracket l ;
        pp_print_space ff () ;
        pp_binop ff op ;
        pp_print_space ff () ;
        bracket r ;
      end ; pp_close_box ff ()
  | Call (f, es) ->
      pp_call ff f es

and pp_call ff f es =
  let open Format in
  pp_open_box ff 2 ; begin
    pp_print_string ff f ;
    pp_print_string ff "(" ;
    pp_print_list ~pp_sep:pp_commaspace pp_expr ff es ;
    pp_print_string ff ")" ;
  end ; pp_close_box ff ()

type vardecl = {
  name : string ;
  ty : ty ;
  mutable init : expr option ;
  pos : Lexing.position ;
}

(** Statements *)
type stmt_ =
  | Declare of vardecl
  (** Variable declaration *)
  | Eval of expr
  (** Evaluate and expression and discard result *)
  | Assign of var * expr
  (** Evaluate expression and store result in var *)
  | Print of expr
  (** Print the value in var *)
  | IfElse of expr * stmt * stmt
  (** Conditionals *)
  | While of expr * stmt
  (** While *)
  | Block of stmt list
  (** Scope *)
  | Return of expr option
  (** Return a result *)

and stmt = {
  stmt : stmt_ ;
  pos : Lexing.position ;
}

let rec pp_stmt ff stmt =
  let open Format in
  match stmt.stmt with
  | Declare vd ->
      pp_vardecl ff vd
  | Eval e ->
      fprintf ff "@[<h>%a;@]" pp_expr e
  | Assign (v, e) ->
      fprintf ff "@[<b2>%a =@ %a;@]"
        pp_var v pp_expr e
  | Print e ->
      fprintf ff "@[<b2>print %a;@]"
        pp_expr e
  | IfElse (cond, th, {stmt = Block [] ; _}) ->
      fprintf ff "@[<h>if (%a) %a@]"
        pp_expr cond pp_stmt th
  | IfElse (cond, th, el) ->
      fprintf ff "@[<h>if (%a) %a else %a@]"
        pp_expr cond pp_stmt th pp_stmt el
  | While (cond, bl) ->
      fprintf ff "@[<h>while (%a) %a@]"
        pp_expr cond pp_stmt bl
  | Block [] ->
      fprintf ff "{ }"
  | Block bl ->
      pp_block ff bl
  | Return None ->
      fprintf ff "return;"
  | Return (Some e) ->
      fprintf ff "@[<b2>return@ %a;@]"
        pp_expr e

and pp_block ff bl =
  Format.fprintf ff "{@\n  @[<v0>%a@]@\n}"
    pp_stmts bl

and pp_stmts ff stmts =
  match stmts with
  | [] -> ()
  | [stmt] -> pp_stmt ff stmt
  | stmt :: stmts ->
      pp_stmt ff stmt ;
      Format.pp_print_space ff ();
      pp_stmts ff stmts

and pp_vardecl ff vd =
  let open Format in
  match vd.init with
  | None ->
      fprintf ff "@[<h>var %s : %a;@]"
        vd.name pp_ty vd.ty
  | Some init ->
      fprintf ff "@[<h>var %s = %a : %a;@]"
        vd.name pp_expr init pp_ty vd.ty

type params = (string * ty) list

type fundecl = {
  name : string ;
  params : params ;
  retty : ty option ;
  body : stmt ;
  pos : Lexing.position ;
}

type prog_unit =
  | Callable of fundecl
  | Global of vardecl

type prog = prog_unit StringTab.t

let rec pp_params ff params =
  let open Format in
  match params with
  | [] -> ()
  | [param] -> pp_param ff param
  | param :: params ->
      pp_param ff param ;
      pp_print_string ff "," ;
      pp_print_space ff () ;
      pp_params ff params

and pp_param ff (v, ty) =
  let open Format in
  fprintf ff "@[<h>%s : %a@]" v pp_ty ty

let pp_prog_unit ff pu =
  let open Format in
  match pu with
  | Callable ({retty = None ; _} as decl) ->
      fprintf ff "@[<v0>proc %s(@[<hv0>%a@])@,%a@]"
        decl.name
        pp_params decl.params
        pp_stmt decl.body
  | Callable ({retty = Some retty ; _ } as decl) ->
      fprintf ff "@[<v0>fun %s(@[<hv0>%a@]) : %a@,%a@]"
        decl.name
        pp_params decl.params
        pp_ty retty
        pp_stmt decl.body
  | Global decl -> begin
      match decl.init with
      | None ->
          fprintf ff "@[<h>var %s : %a;@]"
            decl.name pp_ty decl.ty
      | Some init ->
          fprintf ff "@[<h>var %s = %a : %a;@]"
            decl.name pp_expr init pp_ty decl.ty
    end

let pp_prog ff (prog : prog) =
  let open Format in
  pp_open_vbox ff 0 ; begin
    StringTab.iter begin fun _ pu ->
      pp_prog_unit ff pu ;
      pp_print_cut ff () ;
    end prog ;
  end ; Format.pp_close_box ff () ;
  pp_print_newline ff ()

let pp_pos ff pos =
  let open Lexing in
  match pos with
  | None -> ()
  | Some pos ->
      Format.fprintf ff "in %S, line %d, column %d"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

exception Typecheck of string
let typefailf ?pos fmt =
  Format.kasprintf (fun s -> raise (Typecheck s))
    ("%a@\n" ^^ fmt)
    pp_pos pos

type tyctx = {
  globals : prog_unit StringTab.t ;
  locals : (vardecl * int) StringMap.t ;
}

let lookup_var tyctx v =
  match StringMap.find v tyctx.locals with
  | decldep -> decldep
  | exception Not_found -> begin
      match StringTab.find tyctx.globals v with
      | Global decl -> (decl, 0)
      | _ -> raise Not_found
    end

let lookup_fun tyctx f =
  match StringTab.find tyctx.globals f with
  | Callable decl -> decl
  | _ -> raise Not_found

let rec tycheck_expr tyctx (e : expr) =
  let e0 = e in
  let pos = e.pos in
  let ty = match e.expr with
    | Number _ -> Some Int64
    | Boolean _ -> Some Bool
    | Read (Var v) -> begin
        match lookup_var tyctx v with
        | (vdecl, _) -> Some vdecl.ty
        | exception Not_found ->
            typefailf ~pos "Unknown variable %s" v
      end
    | Unop (op, e) -> begin
        match op, tycheck_expr tyctx e with
        | Negate, Some Int64 -> Some Int64
        | BitNot, Some Int64 -> Some Int64
        | LogNot, Some Bool -> Some Bool
        | _ ->
            typefailf ~pos "Cannot typecheck: %a" pp_expr e0
      end
    | Binop (op, l, r) -> begin
        match op, tycheck_expr tyctx l, tycheck_expr tyctx r with
        | ( Add | Subtract | Multiply | Divide | Modulus
          | BitAnd | BitOr | BitXor | Lshift | Rshift ),
          Some Int64, Some Int64 -> Some Int64
        | ( Lt | Leq | Gt | Geq ), Some Int64, Some Int64 -> Some Bool
        | ( Eq | Neq ), Some lt, Some rt when lt = rt -> Some Bool
        | ( LogAnd | LogOr ), Some Bool, Some Bool -> Some Bool
        | _ ->
            typefailf ~pos "Cannot typecheck: %a" pp_expr e0
      end
    | Call (f, es) -> begin
        match lookup_fun tyctx f with
        | decl ->
            if List.length decl.params <> List.length es then
              typefailf ~pos "Incorrect number of arguments to %s" f ;
            List.iter2 begin fun (v, ty) arg ->
              match tycheck_expr tyctx arg with
              | Some argty ->
                  if argty <> ty then
                    typefailf ~pos "Argument `%s' to %s(): expected %a, got %a"
                      v f pp_ty ty pp_ty argty
              | None ->
                  typefailf ~pos "Argument %s to %s: not an expression" v f
            end decl.params es ;
            decl.retty
        | exception Not_found ->
            typefailf ~pos "Unknown callable %s" f
      end
  in
  e.ty <- ty ; ty

let rec tycheck_stmt ?(depth=0) retty tyctx (stmt : stmt) =
  let pos = stmt.pos in
  match stmt.stmt with
  | Declare vd -> begin
      match lookup_var tyctx vd.name with
      | oldvd, vdepth when depth = vdepth ->
          typefailf ~pos
            "Variable %s shadows another definition in same scope@\nEarlier definition %a"
            vd.name pp_pos (Some oldvd.pos)
      | _
      | exception Not_found ->
          {tyctx with locals = StringMap.add vd.name (vd, depth) tyctx.locals}
    end
  | Eval e ->
      ignore (tycheck_expr tyctx e) ; tyctx
  | Assign (Var v, e) -> begin
      match lookup_var tyctx v with
      | (decl, _) ->
          if tycheck_expr tyctx e <> Some decl.ty then
            typefailf ~pos "Cannot write to a %a variable: %a"
              pp_ty decl.ty pp_expr e ;
          tyctx
      | exception Not_found ->
          typefailf ~pos "Cannot write to undeclared variable %s" v
    end
  | Print e ->
      ignore (tycheck_expr tyctx e) ; tyctx
  | IfElse (cond, th, el) ->
      if tycheck_expr tyctx cond <> Some Bool then
        typefailf ~pos "Condition is not a Boolean expression: %a" pp_expr cond ;
      ignore (tycheck_stmt ~depth retty tyctx th) ;
      ignore (tycheck_stmt ~depth retty tyctx el) ;
      tyctx
  | While (cond, bl) ->
      if tycheck_expr tyctx cond <> Some Bool then
        typefailf ~pos "Condition is not a Boolean expression: %a" pp_expr cond ;
      ignore (tycheck_stmt ~depth retty tyctx bl) ;
      tyctx
  | Block sts ->
      let _ = List.fold_left (tycheck_stmt ~depth:(depth + 1) retty) tyctx sts in
      tyctx
  | Return None ->
      if Option.is_some retty then
        typefailf ~pos "Function needs to return a value" ;
      tyctx
  | Return (Some e) ->
      if tycheck_expr tyctx e <> retty then
        typefailf ~pos "Invalid return statement for this kind of callable" ;
      tyctx

let rec has_return stmt =
  match stmt.stmt with
  | Declare _ | Eval _ | Assign _ | Print _
  | While _ | Return None ->
      false
  | Block sts ->
      List.exists has_return (List.rev sts)
  | IfElse (_, th, el) ->
      has_return th && has_return el
  | Return (Some _) ->
      true

let create_program pus =
  (* pass 1: collect type signatures *)
  let globals = StringTab.create (List.length pus) in
  List.iter begin fun pu ->
    let name = match pu with
      | Callable decl -> decl.name
      | Global decl -> decl.name
    in
    if StringTab.mem globals name then
      typefailf "Multiple definitions of identifier %s" name ;
    StringTab.replace globals name pu
  end pus ;
  (* pass 2: check all the types *)
  StringTab.iter begin fun _ decl ->
    match decl with
    | Global decl -> begin
        match decl.ty, decl.init with
        | Int64, Some {expr = Number _ ; _}
        | Bool, Some {expr = Boolean _ ; _} ->
            ()
        | _ ->
            typefailf ~pos:decl.pos
              "Invalid initializer for global variable %s" decl.name
      end
    | Callable decl ->
        let locals = List.fold_left begin
            fun locals (v, ty) ->
              if StringMap.mem v locals then
                typefailf ~pos:decl.pos
                  "Repeated parameter %s in %s" v decl.name ;
              StringMap.add v ({name = v ; ty = ty ; init = None ;
                                pos = decl.pos}, 1) locals
          end StringMap.empty decl.params in
        let tyctx = {globals ; locals} in
        ignore (tycheck_stmt decl.retty tyctx decl.body) ;
        if Option.is_some decl.retty && not (has_return decl.body) then
          typefailf ~pos:decl.pos "Function %S does not have a return in every code path"
            decl.name
  end globals ;
  globals
