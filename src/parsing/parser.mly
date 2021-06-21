%{
  open ParsedAst
  open Utils
%}

%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token COLON COMMA DOT SEMI ARROW
%token BEGIN END
%token <ParsedAst.label> LNAME
%token <ParsedAst.label> UNAME
%token <ParsedAst.label> GNAME
%token UNDERSCORE AT
%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token <float> FLOAT
%token IMPORT MODULE
%token BREAK CONTINUE RET PRINT THIS NULL
%token FN
%token IF ELIF ELSE
%token LET CONST PUB REF STATIC
%token LOOP WHILE FOR IN
%token CLASS TRAIT IMPL
%token NEW DEL
%token EQUAL LARGER LEQ SMALLER SEQ NEQ
%token ASSIGN MUL_ASSIGN DIV_ASSIGN PLUS_ASSIGN MINUS_ASSIGN MOD_ASSIGN
%token STAR DIVIDE MOD
%token BAR AMPER
%token PLUS MINUS
%token NOT
%token BARBAR AMPERAMPER
%token EOF

%left ARROW IN
%right ASSIGN MUL_ASSIGN DIV_ASSIGN PLUS_ASSIGN MINUS_ASSIGN MOD_ASSIGN
%right NOT
%left BARBAR
%left AMPERAMPER
%left BAR
%left AMBER
%left EQUAL NEQ
%left LARGER LEQ SMALLER SEQ shift
%left PLUS MINUS
%left STAR DIVIDE MOD

%start prog
%type <ParsedAst.program> prog

%%

prog:
  | s = module_clause? i_lst = import_decls d_lst = top_defns EOF
    { Prog (s, i_lst, d_lst) }

module_clause: mark_position(plain_module_clause) { $1 }
plain_module_clause:
  | MODULE lst = separated_nonempty_list(DOT, name) SEMI
    { let mname = String.concat "." lst in Module mname }

import_decls: lst = import_decl* { lst }

import_decl: mark_position(plain_import_decl) { $1 }
plain_import_decl:
  | IMPORT lst = separated_nonempty_list(COMMA, module_path_list) SEMI
    {
      let import_lst = List.concat lst in
      let split xs =
        let rxs = List.rev xs in
        ((String.concat "." (List.rev (List.tl rxs))), List.hd rxs) in
      let module_item_list = List.map split import_lst in
      Import module_item_list
    }
module_path_list:
  | lst = separated_nonempty_list(DOT, name)
    { [lst;] }
  | pre = module_root_path LPAREN lst = separated_nonempty_list(COMMA, module_path) RPAREN
    { List.map (fun x -> pre @ x) lst }
module_root_path:
  | pre = name DOT { [pre;] }
  | pre = name DOT sub = module_root_path { pre :: sub }
module_path:
  | n = name { [n;] }
  | n = name DOT lst = module_path { n :: lst }

top_defns:
  | { [] }
  | def = definition SEMI lst = top_defns { def :: lst }

lname: LNAME { $1 }
uname: UNAME { $1 }
name:
  | n = lname
    { n }
  | n = uname
    { n }

definition:
  | t = ty_def
    { t }
  | v = vars_decl
    { v }
  | f = function_def
    { f }

expression: mark_position(plain_expression) { $1 }

plain_expression:
  | v = plain_vars_decl
    { v }
  | e = expression DOT id = name
    { Retrive (e, id) }
  | e = expression LBRACK id = expression RBRACK
    { Index (e, id) }
  | NEW n = uname lst = function_call_params 
    { New (n, [], lst) }
  | NEW n = uname lst1 = gen_ty_consume lst2 = function_call_params 
    { New (n, lst1, lst2) }
  | DEL n = lname
    { Del n }
  | LPAREN e = plain_expression RPAREN
    { e }
  | LBRACK RBRACK
    { Array [] }
  | LBRACK lst = separated_nonempty_list(COMMA, expression) RBRACK
    { Array lst }
  | op = un_op e = expression
    { UnOp (op, e) }
  | e1 = expression op = bin_op e2 = expression
    { BinOp (op, e1, e2) }
  | e1 = expression op = assign_op e2 = expression
    { AssignOp (op, e1, e2) }
  | BREAK e = expression
    { Break e }
  | CONTINUE e = expression
    { Continue e }
  | RET e = expression
    { Return e }
  | PRINT LPAREN e = expression RPAREN
    { Print e }
  | e = expression DOT f = name lst = function_call_params
    { MethodCall (e, f, lst) }
  | f = name lst = function_call_params
    { Call (f, lst) }
  | e = expression_with_blk
    { e }
  | i = literal
    { Literal i }
  | THIS
    { This }
  | n = name
    { Id n }

expression_with_blk:
  | BEGIN b = block END
    { b }
  | f = plain_function_def
    { f }
  | e = if_expression
    { e }
  | LOOP b = block END
    { Loop b }
  | WHILE e = expression COLON b = block END
    { While (e, b) }
  | FOR n = lname IN e = expression COLON b = block END
    { For (n, e, b) }

%inline assign_op:
  | MUL_ASSIGN   { MulAssign }
  | DIV_ASSIGN   { DivAssign }
  | MOD_ASSIGN   { ModAssign }
  | PLUS_ASSIGN  { PlusAssign }
  | MINUS_ASSIGN { MinusAssign }
  | ASSIGN       { Assign }
%inline un_op:
  | NOT          { UnOpNot }
  | MINUS        { UnOpNeg }
%inline bin_op:
  | PLUS         { BinOpPlus }
  | MINUS        { BinOpMinus }
  | STAR         { BinOpMult }
  | DIVIDE       { BinOpDiv } 
  | MOD          { BinOpMod }
  | LARGER       { BinOpLarger }
  | LEQ          { BinOpLeq }
  | SMALLER      { BinOpSmaller }
  | SEQ          { BinOpSeq }
  | AMPERAMPER   { BinOpAnd }
  | BARBAR       { BinOpOr }
  | EQUAL        { BinOpEq }
  | NEQ          { BinOpNeq }

if_expression:
  | IF e1 = expression COLON b1 = block lst = list(elif_expression) els = option(else_expression) END
    { let els_lst =
        match els with
        | Some e -> [e]
        | None -> [] in
      If (Cond (e1, b1) :: lst @ els_lst) }
elif_expression:
  | ELIF e = expression COLON b = block
    { Cond (e, b) }
else_expression:
  | ELSE COLON e = block
    { Else e }

vars_decl: mark_position(plain_vars_decl) { $1 }
plain_vars_decl:
  | LET lst = separated_nonempty_list(COMMA, var_decl)
    { Vars lst }
var_decl: mark_position(plain_var_decl) { $1 }
plain_var_decl:
  | d = var_decorator name = LNAME COLON ty = ty
    { Var (name, d, Some ty, None) }
  | d = var_decorator name = LNAME COLON ty = ty ASSIGN exp = expression
    { Var (name, d, Some ty, Some exp) }
  | d = var_decorator name = LNAME ASSIGN exp = expression
    { Var (name, d, None, Some exp) }
var_decorator:
  | c = CONST?
    {
      let jg = function
        | None -> false
        | Some _ -> true
      in { const= jg c; }
    }

function_def: mark_position(plain_function_def) { $1 }
plain_function_def:
  | FN n = LNAME lst = function_params ARROW b = block END
    { Fn (n, [], lst, None, b) }
  | FN n = LNAME lst = function_params COLON t = ty ARROW b = block END
    { Fn (n, [], lst, Some t, b) }
  | FN n = LNAME lst1 = gen_ty_def lst2 = function_params ARROW b = block END
    { Fn (n, lst1, lst2, None, b) }
  | FN n = LNAME lst1 = gen_ty_def lst2 = function_params COLON t = ty ARROW b = block END
    { Fn (n, lst1, lst2, Some t, b) }

function_params:
  | LPAREN RPAREN
  { [] }
  | LPAREN lst = function_param_list RPAREN
  { lst }

function_param_list:
  | p = param_def { [p;] }
  | p = param_def COMMA lst = function_param_list { p :: lst }

/*| LPAREN RPAREN
    { [] }
  | LPAREN p = param_def lst1 = comma_param_def* lst2 = com.ma_param_def_default* RPAREN
    { p::lst1@lst2 }
  | LPAREN p = param_def_default lst = comma_param_def_default* RPAREN
    { p::lst }
  | lst1 = separated_nonempty_list(COMMA, param_def) COMMA lst2 = separated_list(COMMA, param_def_default)
    { lst1 @ lst2 }
  | lst = separated_nonempty_list(COMMA, param_def_default)
    { lst } */


param_def:
  | d = var_decorator n = lname COLON t = ty
    { Param (n, d, Some t, None) }
  | d = var_decorator n = lname
    { Param (n, d, None, None) }
  | d = var_decorator n = lname COLON t = ty ASSIGN exp = expression
    { Param (n, d, Some t, Some(exp)) }
  | d = var_decorator n = lname ASSIGN exp = expression
    { Param (n, d, None, Some(exp)) }

function_call_params:
  | LPAREN lst = separated_list(COMMA, param_call) RPAREN
    { lst }

param_call:
  | v = expression
    { CallParam (None, v) }
  | name = LNAME ASSIGN v = expression
    { CallParam (Some(name), v) }

block:
  |
    { Block [] }
  | e = expression SEMI b = block
    { match b with | Block lst -> Block (e :: lst) | _ -> Block [e;] }

ty_def:
  | c = class_def
    { c }
  | t = trait_def
    { t }
  | i = impl_def
    { i }

ty: mark_position(plain_ty) { $1 }
plain_ty:
  | t = ty_apply
    { t }

ty_apply:
  | t1 = tyname SMALLER ts = separated_nonempty_list(COMMA, ty) LARGER
    { TyApply (t1, ts) }
  | t = tyname
    { TyApply (t, []) }
  | t = genname
    { TyApply (t, []) }

tyname: t = name { t }
genname: t = GNAME { t }

gen_ty_def: SMALLER lst = gen_ty_def_list LARGER { lst }
gen_ty_def_list:
  | t = gen_ty { [t;] }
  | t = gen_ty COMMA lst = gen_ty_def_list { t :: lst }
gen_ty: mark_position(plain_gen_ty) { $1 }
plain_gen_ty:
  | n = genname  { GenTy n }
  | n = genname COLON t = ty { CstGenTy (n, t) }

gen_ty_consume: SMALLER lst = gen_ty_consume_list LARGER { lst }
gen_ty_consume_list:
  | t = ty { [t;] }
  | t = ty COMMA lst = gen_ty_consume_list { t :: lst }

class_def: mark_position(plain_class_def) { $1 }
plain_class_def:
  | CLASS h = class_head COLON lst = class_body END
  { let (n, gen_lst, f)= h in Class (n, f, gen_lst, lst) }
class_head:
  | n = uname f = class_father? { (n, [], f) }
  | n = uname lst = gen_ty_def f = class_father? { (n, lst, f) }
class_father: 
  | LPAREN f = uname RPAREN { (f, []) }
  | LPAREN f = uname lst = gen_ty_consume RPAREN { (f, lst) }
class_body:
  |
  { [] }
  | d = field_decl SEMI lst = class_body
  { d :: lst }

field_decl: mark_position(plain_field_decl) { $1 }
plain_field_decl:
  | lst = separated_nonempty_list(COMMA, properties_decl)
  { Properties lst }
  | m = method_decl
  { m }

method_decl:
  | d = field_decorator FN n = lname lst = function_params ARROW b = block END
    { Method (n, d, [], lst, None, b) }
  | d = field_decorator FN n = lname lst = function_params COLON t = ty ARROW b = block END
    { Method (n, d, [], lst, Some t, b) }
  | d = field_decorator FN n = lname lst1 = gen_ty_def lst2 = function_params ARROW b = block END
    { Method (n, d, lst1, lst2, None, b) }
  | d = field_decorator FN n = lname lst1 = gen_ty_def lst2 = function_params COLON t = ty ARROW b = block END
    { Method (n, d, lst1, lst2, Some t, b) }

properties_decl:
  | anns = field_decorator name = LNAME COLON t = ty
    { Property (name, anns, Some t, None) }
  | anns = field_decorator name = LNAME COLON t = ty ASSIGN exp = expression
    { Property (name, anns, Some t, Some exp) }
  | anns = field_decorator name = LNAME ASSIGN exp = expression
    { Property (name, anns, None, Some exp) }
field_decorator:
  | s = STATIC? p = PUB? c = CONST?
    {
      let jg = function
        | None -> false
        | Some _ -> true
      in { static= jg s; pub= jg p; const= jg c; }
    }

trait_def: mark_position(plain_trait_def) { $1 }
plain_trait_def:
  | TRAIT n = uname COLON lst = class_body END
    { Trait (n, [], lst) }
  | TRAIT n = uname lst1 = gen_ty_def COLON lst2 = class_body END
    { Trait (n, lst1, lst2) }

impl_def: mark_position(plain_impl_def) { $1 }
plain_impl_def:
  | CLASS c = uname IMPL t = uname COLON lst = class_body END
  { Impl (c, t, [], [], lst) }
  | CLASS c = uname lst1 = gen_ty_def IMPL t = uname COLON lst2 = class_body END
  { Impl (c, t, lst1, [], lst2) }
  | CLASS c = uname IMPL t = uname lst1 = gen_ty_consume COLON lst2 = class_body END
  { Impl (c, t, [], lst1, lst2) }
  | CLASS c = uname lst1 = gen_ty_def IMPL t = uname lst2 = gen_ty_consume COLON lst3 = class_body END
  { Impl (c, t, lst1, lst2, lst3) }

mark_position(X):
  x = X
  { { it = x; at = Location.make $startpos $endpos } }

literal:
  | n = INT
    { IntVal n }
  | str = STRING
    { StrVal str }
  | b = BOOL
    { BoolVal b }
  | f = FLOAT
    { FloatVal f }
  | NULL
    { Null }
