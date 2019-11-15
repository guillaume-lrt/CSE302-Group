open Ast
open Utils

exception Execution_failure of string

let execfail fmt =
  Format.kasprintf
    (fun s -> raise (Execution_failure s))
    fmt

exception Return of expr option

let rec interpret_call tyctx f args =
  match StringTab.find tyctx.globals f with
  | Callable decl ->
      let locals =
        List.fold_left2 begin fun locals (v, ty) (arg : expr) ->
          let vd : vardecl = {
            name = v ; ty = ty ;
            init = Some (interpret_expr tyctx arg) ;
            pos = arg.pos ;
          } in
          StringMap.add v (vd, 0) locals
        end StringMap.empty decl.params args in
      let inproc = Option.is_none decl.retty in
      interpret_stmt inproc {tyctx with locals} decl.body
  | _ ->
      execfail "Callable %S not found" f

and interpret_proc_call tyctx f args =
  try
    (* let args = List.map (interpret_expr tyctx) args in
     * debug "proc call: %t@." (fun ff -> pp_call ff f args) ; *)
    ignore (interpret_call tyctx f args) ;
    tyctx
  with
  | Return None -> tyctx
  | Return (Some res) ->
      execfail "Calling proc %S returned: %a" f pp_expr res

and interpret_fun_call tyctx f args =
  try
    (* let args = List.map (interpret_expr tyctx) args in
     * debug "fun call: %t@." (fun ff -> pp_call ff f args) ; *)
    ignore (interpret_call tyctx f args) ;
    execfail "Call to %S did not return" f
  with
  | Return (Some res) -> res
  | Return None ->
      execfail "Call to %S did not return a value" f

and interpret_stmt inproc tyctx stmt =
  match stmt.stmt with
  | Block stmts ->
      let _ = List.fold_left (interpret_stmt inproc) tyctx stmts in
      tyctx
  | Declare decl ->
      let decl = {
        decl with
        init = Option.map (interpret_expr tyctx) decl.init
      } in
      {tyctx with locals = StringMap.add decl.name (decl, 0) tyctx.locals}
  | Eval {expr = Call (f, args) ; _} when inproc ->
      interpret_proc_call tyctx f args
  | Eval e ->
      let _ = interpret_expr tyctx e in
      tyctx
  | Assign (Var v, e) ->
      let result = interpret_expr tyctx e in
      (lookup_var tyctx v |> fst).init <- Some result ;
      tyctx
  | Print e ->
      let result = interpret_expr tyctx e in
      interpret_print result ;
      tyctx
  | IfElse (cond, th, el) -> begin
      match interpret_expr tyctx cond with
      | {expr = Boolean true ; _} ->
          interpret_stmt inproc tyctx th
      | {expr = Boolean false ; _} ->
          interpret_stmt inproc tyctx el
      | res ->
          execfail "Got unexpected result: %a@\nWhile interpreting: %a"
            pp_expr res pp_expr cond
    end
  | While (cond, bod) -> begin
      match interpret_expr tyctx cond with
      | {expr = Boolean true ; _} ->
          let _ = interpret_stmt inproc tyctx bod in
          interpret_stmt inproc tyctx stmt
      | {expr = Boolean false ; _} ->
          tyctx
      | res ->
          execfail "Got unexpected result: %a@\nWhile interpreting: %a"
            pp_expr res pp_expr cond
    end
  | Return (Some {expr = Call (f, args) ; _}) when inproc ->
      let _ = interpret_proc_call tyctx f args in
      raise (Return None)
  | Return None when inproc ->
      raise (Return None)
  | Return (Some e) ->
      raise (Return (Some (interpret_expr tyctx e)))
  | Return None ->
      execfail "Missing required argument to return"

and interpret_print res =
  match res.expr with
  | Number n -> Printf.printf "%d\n" n
  | Boolean b -> Printf.printf "%b\n" b
  | _ ->
      execfail "Not a value: %a" pp_expr res

and interpret_expr tyctx e =
  let e0 = e in
  match e.expr with
  | Number _ | Boolean _ -> e
  | Read (Var v) -> begin
      match lookup_var tyctx v |> fst with
      | { init = Some e ; _ } ->
          interpret_expr tyctx e
      | { init = None ; _ } ->
         execfail "Uninitialized variable: %s" v
      | exception Not_found ->
         execfail "Unknown variable: %s" v
    end
  | Unop (op, e) -> begin
      match op, interpret_expr tyctx e with
      | Negate, ({expr = Number n ; _} as e) ->
          {e with expr = Number (- n)}
      | BitNot, ({expr = Number n ; _} as e) ->
          {e with expr = Number (Int.lognot n)}
      | LogNot, ({expr = Boolean b ; _} as e) ->
          {e with expr = Boolean (not b)}
      | _ ->
          execfail "Type error: %a" pp_expr e
    end
  | Binop ((LogAnd | LogOr) as op, l, r) -> begin
      match op, (interpret_expr tyctx l).expr with
      | LogAnd, Boolean false ->
          {e0 with expr = Boolean false}
      | LogAnd, Boolean true ->
          interpret_expr tyctx r
      | LogOr, Boolean true ->
          {e0 with expr = Boolean true}
      | LogOr, Boolean false ->
          interpret_expr tyctx r
      | _ ->
          execfail "First arg of logical operator: %a" pp_expr l
    end
  | Binop (op, l, r) -> begin
      match op, (interpret_expr tyctx l).expr, (interpret_expr tyctx r).expr with
      | Add, Number m, Number n ->
          {e0 with expr = Number (m + n)}
      | Subtract, Number m, Number n ->
          {e0 with expr = Number (m - n)}
      | Multiply, Number m, Number n ->
          {e0 with expr = Number (m * n)}
      | Divide, Number m, Number n ->
          {e0 with expr = Number (m / n)}
      | Modulus, Number m, Number n ->
          {e0 with expr = Number (m mod n)}
      | BitAnd, Number m, Number n ->
          {e0 with expr = Number (Int.logand m n)}
      | BitOr, Number m, Number n ->
          {e0 with expr = Number (Int.logor m n)}
      | BitXor, Number m, Number n ->
          {e0 with expr = Number (Int.logxor m n)}
      | Lshift, Number m, Number n ->
          {e0 with expr = Number (Int.shift_left m n)}
      | Rshift, Number m, Number n ->
          {e0 with expr = Number (Int.shift_right m n)}
      | Eq, Number m, Number n ->
          {e0 with expr = Boolean (m = n)}
      | Eq, Boolean b, Boolean c ->
          {e0 with expr = Boolean (b = c)}
      | Neq, Number m, Number n ->
          {e0 with expr = Boolean (m <> n)}
      | Neq, Boolean b, Boolean c ->
          {e0 with expr = Boolean (b <> c)}
      | Lt, Number m, Number n ->
          {e0 with expr = Boolean (m < n)}
      | Leq, Number m, Number n ->
          {e0 with expr = Boolean (m <= n)}
      | Gt, Number m, Number n ->
          {e0 with expr = Boolean (m > n)}
      | Geq, Number m, Number n ->
          {e0 with expr = Boolean (m >= n)}
      | _ ->
          execfail "Type error: %a" pp_expr e0
    end
  | Call (f, args) ->
      interpret_fun_call tyctx f args
