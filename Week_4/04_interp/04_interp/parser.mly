%{

let untyped pos expr = Ast.{expr ; ty = None ; pos}

let placeat pos (stmt : Ast.stmt_) = Ast.{stmt ; pos}

%}

%token <string> IDENT
%token <int> INT
%token <bool> BOOL
%token TY_INT64 TY_BOOL
%token PLUS MINUS ASTERISK SLASH PERCENT
%token AMP BAR CARET LTLT GTGT
%token UMINUS TILDE
%token EQEQ BANGEQ LT LTEQ GT GTEQ
%token AMPAMP BARBAR BANG
%token FUN PROC RETURN
%token EQ PRINT VAR IF ELSE WHILE
%token SEMICOLON COMMA COLON LPAREN RPAREN LBRACE RBRACE
%token ENDOFSTREAM

%left BARBAR
%left AMPAMP
%left BAR
%left CARET
%left AMP
%nonassoc EQEQ BANGEQ
%nonassoc LT LTEQ GT GTEQ
%left LTLT GTGT
%left PLUS MINUS
%left ASTERISK SLASH PERCENT
%nonassoc UMINUS

%start <Ast.prog_unit Utils.StringTab.t> prog

%%

prog:
| pus=list(prog_unit) ENDOFSTREAM
{ Ast.(create_program (List.concat pus)) }

prog_unit:
| gv=vardecl
{ List.map (fun vd -> Ast.Global vd) gv }
| fd=fundecl
{ [fd] }
| pd=procdecl
{ [pd] }

vardecl:
| VAR vars=separated_nonempty_list(COMMA, varinit) COLON ty=ty SEMICOLON
{ List.fold_right begin
      fun (var, init, pos) vs ->
        Ast.{name = var ; ty ; init ; pos} :: vs
  end vars [] }

varinit:
| x=IDENT init=option(EQ e=expr {e})
{ (x, init, $symbolstartpos) }

fundecl:
| FUN f=IDENT LPAREN params=separated_list(COMMA, param) RPAREN COLON retty=ty bl=block
{ Ast.(Callable {name = f ; params = List.concat params ;
                 retty = Some retty ; body = bl ;
                 pos = $symbolstartpos}) }

procdecl:
| PROC f=IDENT LPAREN params=separated_list(COMMA, param) RPAREN bl=block
{ Ast.(Callable {name = f ; params = List.concat params ;
                 retty = None ; body = bl ;
                 pos = $symbolstartpos}) }

param:
| vs=separated_nonempty_list(COMMA, IDENT) COLON ty=ty
{ List.map (fun v -> (v, ty)) vs }

ty:
| TY_INT64
{ Ast.Int64 }
| TY_BOOL
{ Ast.Bool }

expr:
| n=INT
{ untyped $symbolstartpos Ast.(Number n) }
| b=BOOL
{ untyped $symbolstartpos Ast.(Boolean b) }
| v=IDENT
{ untyped $symbolstartpos Ast.(Read (Var v)) }
| f=IDENT LPAREN args=separated_list(COMMA, expr) RPAREN
{ untyped $symbolstartpos Ast.(Call (f, args)) }
| e1=expr PLUS e2=expr
{ untyped $symbolstartpos Ast.(Binop (Add, e1, e2)) }
| e1=expr MINUS e2=expr
{ untyped $symbolstartpos Ast.(Binop (Subtract, e1, e2)) }
| e1=expr ASTERISK e2=expr
{ untyped $symbolstartpos Ast.(Binop (Multiply, e1, e2)) }
| e1=expr SLASH e2=expr
{ untyped $symbolstartpos Ast.(Binop (Divide, e1, e2)) }
| e1=expr PERCENT e2=expr
{ untyped $symbolstartpos Ast.(Binop (Modulus, e1, e2)) }
| e1=expr LTLT e2=expr
{ untyped $symbolstartpos Ast.(Binop (Lshift, e1, e2)) }
| e1=expr GTGT e2=expr
{ untyped $symbolstartpos Ast.(Binop (Rshift, e1, e2)) }
| e1=expr AMP e2=expr
{ untyped $symbolstartpos Ast.(Binop (BitAnd, e1, e2)) }
| e1=expr BAR e2=expr
{ untyped $symbolstartpos Ast.(Binop (BitOr, e1, e2)) }
| e1=expr CARET e2=expr
{ untyped $symbolstartpos Ast.(Binop (BitXor, e1, e2)) }
| e1=expr EQEQ e2=expr
{ untyped $symbolstartpos Ast.(Binop (Eq, e1, e2)) }
| e1=expr BANGEQ e2=expr
{ untyped $symbolstartpos Ast.(Binop (Neq, e1, e2)) }
| e1=expr LT e2=expr
{ untyped $symbolstartpos Ast.(Binop (Lt, e1, e2)) }
| e1=expr LTEQ e2=expr
{ untyped $symbolstartpos Ast.(Binop (Leq, e1, e2)) }
| e1=expr GT e2=expr
{ untyped $symbolstartpos Ast.(Binop (Gt, e1, e2)) }
| e1=expr GTEQ e2=expr
{ untyped $symbolstartpos Ast.(Binop (Geq, e1, e2)) }
| e1=expr AMPAMP e2=expr
{ untyped $symbolstartpos Ast.(Binop (LogAnd, e1, e2)) }
| e1=expr BARBAR e2=expr
{ untyped $symbolstartpos Ast.(Binop (LogOr, e1, e2)) }
| MINUS e=expr %prec UMINUS
{ untyped $symbolstartpos Ast.(Unop (Negate, e)) }
| TILDE e=expr %prec UMINUS
{ untyped $symbolstartpos Ast.(Unop (BitNot, e)) }
| BANG e=expr %prec UMINUS
{ untyped $symbolstartpos Ast.(Unop (LogNot, e)) }
| LPAREN e=expr RPAREN
{ e }

stmt:
| d=IDENT EQ e=expr SEMICOLON
{ [placeat $symbolstartpos Ast.(Assign (Var d, e))] }
| PRINT e=expr SEMICOLON
{ [placeat $symbolstartpos Ast.(Print e)] }
| decls=vardecl
{ Ast.(List.map (fun vd -> placeat $symbolstartpos (Declare vd)) decls) }
| b=block
{ [b] }
| s=ifelse
{ [s] }
| WHILE LPAREN cond=expr RPAREN body=block
{ [placeat $symbolstartpos Ast.(While (cond, body))] }
| RETURN arg=option(expr) SEMICOLON
{ [placeat $symbolstartpos Ast.(Return arg)] }
| e=expr SEMICOLON
{ [placeat $symbolstartpos Ast.(Eval e)] }

ifelse: IF LPAREN cond=expr RPAREN trubl=block falbl=option(ELSE cont=ifcont { cont })
{ placeat $symbolstartpos
    Ast.(IfElse (cond, trubl,
                 Option.value falbl ~default:(placeat $symbolstartpos (Block [])))) }

%inline ifcont:
| s=ifelse
{ s }
| b=block
{ b }

%inline block:
| LBRACE bl=list(stmt) RBRACE
{ placeat $symbolstartpos Ast.(Block (List.concat bl)) }
