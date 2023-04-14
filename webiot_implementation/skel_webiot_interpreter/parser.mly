%{
    open Webi_interpreter
%}

%token <string> IDENT
%token <int> NUM
%token <string> STR
%token CLIENT SERVICE NOTIMPLEMENTED
%token EQ LEQ GEQ AND OR XOR NOT
%token PLUS MINUS TIMES
%token TILDE DOLLAR
%token WHILE IF FI THEN ELSE SKIP RET ACT GET CALL
%token TRUE FALSE UNDEFINED
%token SEMICOLON LPAR RPAR LPB RPB COMMA ASS SRVS
%token EOF

%start <Webi_interpreter.WEBI_Interp.webiLanguage> webis

%%

webis:
  | p = webi EOF {p}
  | p1 = webi p2 = webis EOF {Webi_interpreter.WEBI_Interp.WL (p1,p2)}

webi:
  | SERVICE url = IDENT SRVS hostname = IDENT LPAR x = IDENT RPAR ASS p = sr_stmts {Webi_interpreter.WEBI_Interp.Service (url,((x,p),hostname))}
  | CLIENT i = NUM CALL LPAR url = IDENT RPAR ASS v = cl_val {Webi_interpreter.WEBI_Interp.Client ((i,url),v)}

sr_stmts:
  | p = sr_stmt {p}
  | p1 = sr_stmt SEMICOLON p2 = sr_stmts {Webi_interpreter.T.Pure (Webi_interpreter.T.Seq (p1,p2))}

sr_stmt:
  | x = IDENT ASS e = sr_expr {Webi_interpreter.T.Pure (Webi_interpreter.T.Ass (x,e))}
  | IF LPAR e = sr_expr RPAR THEN p1 = sr_stmts ELSE p2 = sr_stmts FI {Webi_interpreter.T.Pure (Webi_interpreter.T.Cond (e,p1,p2))}
  | WHILE LPAR e = sr_expr RPAR LPB p = sr_stmts RPB {Webi_interpreter.T.Pure (Webi_interpreter.T.While (e,p))}
  | SKIP {Webi_interpreter.T.Pure (Webi_interpreter.T.Skip)}
  | RET e = sr_expr {Webi_interpreter.T.Pure (Webi_interpreter.T.Ret e)}
  | ACT LPAR a = IDENT COMMA LPAR n = IDENT COMMA l = IDENT RPAR COMMA p = STR RPAR {Webi_interpreter.T.ExtSTMT (Webi_interpreter.S.Act (a,(n,l),p))}
  | GET LPAR x = IDENT COMMA LPAR LPAR n = IDENT COMMA l = IDENT RPAR COMMA p = STR RPAR RPAR {Webi_interpreter.T.ExtSTMT (Webi_interpreter.S.Get (x,((n,l),p)))}

sr_expr:
  | v = sr_val {Webi_interpreter.T.Expr (Webi_interpreter.T.Value v)}
  | x = IDENT {Webi_interpreter.T.Expr (Webi_interpreter.T.Var x)}
  | e1 = sr_expr o = _bop e2 = sr_expr {Webi_interpreter.T.Expr (Webi_interpreter.T.BOp (o,e1,e2))}
  | LPAR e1 = sr_expr RPAR o = _bop LPAR e2 = sr_expr RPAR {Webi_interpreter.T.Expr (Webi_interpreter.T.BOp (o,e1,e2))}
  | o = _uop LPAR e = sr_expr RPAR {Webi_interpreter.T.Expr (Webi_interpreter.T.UOp (o,e))}
  | TILDE LPAR s = t_stmts RPAR {Webi_interpreter.T.ExtExpr (Webi_interpreter.S.Tilde s)}

_bop:
  | PLUS {Webi_interpreter.T.Add}
  | MINUS {Webi_interpreter.T.Sub}
  | TIMES {Webi_interpreter.T.Mul}
  | LEQ {Webi_interpreter.T.LEq}
  | GEQ {Webi_interpreter.T.GEq}
  | EQ {Webi_interpreter.T.Eq}
  | AND {Webi_interpreter.T.AND}
  | OR  {Webi_interpreter.T.OR}
  | XOR {Webi_interpreter.T.XOR}

_uop:
  | NOT {Webi_interpreter.T.Neg}

sr_val:
  | n = NUM {Webi_interpreter.T.Std (Webi_interpreter.T.Nat n)}
  | TRUE {Webi_interpreter.T.Std (Webi_interpreter.T.Bool Webi_interpreter.T.T)}
  | FALSE {Webi_interpreter.T.Std (Webi_interpreter.T.Bool Webi_interpreter.T.F)}
  | UNDEFINED {Webi_interpreter.T.Std (Webi_interpreter.T.Undefined)}
  | s = STR {Webi_interpreter.T.Std (Webi_interpreter.T.Str s)}
  | LPAR RPAR {Webi_interpreter.T.Std Webi_interpreter.T.Unit}
  | s = t_stmts {Webi_interpreter.T.ExtValue (Webi_interpreter.S.TildeClos s)}

t_stmts:
  | p = t_stmt {p}
  | p1 = t_stmt SEMICOLON p2 = t_stmts {Webi_interpreter.T.Pure (Webi_interpreter.T.Seq (p1,p2))}

t_stmt:
  | x = IDENT ASS e = t_expr {Webi_interpreter.T.Pure (Webi_interpreter.T.Ass (x,e))}
  | IF LPAR e = t_expr RPAR THEN p1 = t_stmts ELSE p2 = t_stmts FI {Webi_interpreter.T.Pure (Webi_interpreter.T.Cond (e,p1,p2))}
  | WHILE LPAR e = t_expr RPAR LPB p = t_stmts RPB {Webi_interpreter.T.Pure (Webi_interpreter.T.While (e,p))}
  | SKIP {Webi_interpreter.T.Pure (Webi_interpreter.T.Skip)}
  | RET e = t_expr {Webi_interpreter.T.Pure (Webi_interpreter.T.Ret e)}
  | CALL LPAR u = IDENT COMMA v = dollar_or_val COMMA LPAR x = IDENT COMMA b = t_stmts RPAR RPAR {Webi_interpreter.T.ExtSTMT (Webi_interpreter.S.T_Call (u,v,(x,b)))}

cl_val:
  | n = NUM {Webi_interpreter.T.Std (Webi_interpreter.T.Nat n)}
  | TRUE {Webi_interpreter.T.Std (Webi_interpreter.T.Bool Webi_interpreter.T.T)}
  | FALSE {Webi_interpreter.T.Std (Webi_interpreter.T.Bool Webi_interpreter.T.F)}
  | UNDEFINED {Webi_interpreter.T.Std (Webi_interpreter.T.Undefined)}
  | s = STR {Webi_interpreter.T.Std (Webi_interpreter.T.Str s)}
  | LPAR RPAR {Webi_interpreter.T.Std Webi_interpreter.T.Unit}
  | NOTIMPLEMENTED {Webi_interpreter.T.ExtValue (Webi_interpreter.T.NotImplemented)}

dollar_or_val:
  | e = t_expr {Webi_interpreter.S.D e}
  | v = cl_val {Webi_interpreter.S.V v}

t_expr:
  | v = cl_val {Webi_interpreter.T.Expr (Webi_interpreter.T.Value v)}
  | x = IDENT {Webi_interpreter.T.Expr (Webi_interpreter.T.Var x)}
  | e1 = t_expr o = _bop e2 = t_expr {Webi_interpreter.T.Expr (Webi_interpreter.T.BOp (o,e1,e2))}
  | o = _uop LPAR e = t_expr RPAR {Webi_interpreter.T.Expr (Webi_interpreter.T.UOp (o,e))}
  | DOLLAR x = IDENT {Webi_interpreter.T.ExtExpr (Webi_interpreter.S.Dollar x)}
