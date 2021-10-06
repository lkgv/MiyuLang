%{
  open ParsedAst
  open Utils
  open Ast.AstTypes
%}

%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token COLON COMMA DOT SEMI ARROW
%token BEGIN END
%token <string> LNAME
%token <string> UNAME
%token <string> GNAME
%token AT
%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token <float> FLOAT
%token IMPORT PACKAGE
%token BREAK CONTINUE RET PRINT THIS NULL
%token FN COLONCOLON
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
%left AMPER
%left EQUAL NEQ
%left LARGER LEQ SMALLER SEQ
%left PLUS MINUS
%left STAR DIVIDE MOD
%nonassoc COLONCOLON

%start prog
%type <ParsedAst.program> prog

%%

lname: LNAME { $1 }
uname: UNAME { $1 }
name:
  | n = lname
    { n }
  | n = uname
    { n }

prog:
  | s = module_clause? lst = pkg_body EOF
    { Prog (s, lst) }

sep: single_sep+ {}
single_sep: SEMI {}

module_clause: mark_position(plain_module_clause) { $1 }
plain_module_clause:
  | PACKAGE COLON lst = separated_nonempty_list(DOT, name) sep
    { let mname = String.concat "." lst in Module (Mod_id.of_string mname) }

import_decl: mark_position(plain_import_decl) { $1 }
plain_import_decl:
  | IMPORT lst = separated_nonempty_list(COMMA, module_path_list)
    {
      let import_lst = List.concat lst in
      let split xs =
        let rxs = List.rev xs in
        ((String.concat "." (List.rev (List.tl rxs))), List.hd rxs)
        |> fun (m, n) -> (Mod_id.of_string m, n) in
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

pkg_def: mark_position(plain_pkg_def) { $1 }
plain_pkg_def:
  | PACKAGE n = uname COLON lst = pkg_body END
    { Package (n, lst) }

pkg_body:
  | { [] }
  | d = pkg_field_decl { [d;] }
  | d = pkg_field_decl sep lst = pkg_body { d :: lst }

pkg_field_decl:
  | im = import_decl
    { im }
  | c = class_def
    { c }
  | t = trait_def
    { t }
  | i = impl_def
    { i }
  | v = vars_decl
    { v }
  | f = function_def
    { f }

expression: mark_position(plain_expression) { $1 }
plain_expression:
  | v = plain_vars_decl
    { v }
  | f = plain_function_def
    { f }
  | BEGIN b = block END
    { b }
  | LOOP b = block END
    { Loop b }
  | WHILE e = arithmetic_expression COLON b = block END
    { While (e, b) }
  | FOR n = lname IN e = unary_expression COLON b = block END
    { For ((Var_id.of_string n), e, b) }
  | DEL n = lname
    { Del (Var_id.of_string n) }
  | e = if_expression
    { e }
  | BREAK LPAREN e = arithmetic_expression
    { Break e }
  | CONTINUE e = arithmetic_expression
    { Continue e }
  | RET e = arithmetic_expression
    { Return e }
  | PRINT LPAREN e = arithmetic_expression RPAREN
    { Print e }
  | e = arithmetic_expression { e.it }

arithmetic_expression:
  | e = assignment { e }
  | e = bin_expression { e }

assignment: mark_position(plain_assignment) { $1 }
plain_assignment:
  | e1 = unary_expression op = assign_op e2 = arithmetic_expression
    { AssignOp (op, e1, e2) }

bin_expression:
  | ue = unary_expression { ue }
  | e1 = bin_expression op = bin_op e2 = bin_expression
    { {it= BinOp (op, e1, e2) ;at= Location.make $startpos $endpos} }

unary_expression:
  | pe = primary_expression { pe }
  | op = un_op ue = unary_expression
    { {it= UnOp (op, ue) ;at= Location.make $startpos $endpos} }

primary_expression: mark_position(plain_primary_expression) { $1 }
plain_primary_expression:
  | e = primary_expression_access
    { e }
  | e = primary_expression LBRACK id = arithmetic_expression RBRACK
    { Index (e, id) }

primary_expression_access:
  | pe = primary_expression_start
    { pe }
  | pe = primary_expression DOT id = lname
    { Retrive (pe, (Property_id.of_string id), []) }
  | pe = primary_expression DOT f = lname lst = function_call_params
    { MethodCall (pe, (Method_id.of_string f), [], lst) }
  | f = lname lst = function_call_params
    { Call ((Fn_id.of_string f), [], lst) }
  | pe = primary_expression DOT id = lname COLONCOLON plst = gen_ty_consume
    { Retrive (pe, (Property_id.of_string id), plst) }
  | pe = primary_expression DOT f = lname lst = function_call_params COLONCOLON plst = gen_ty_consume
    { MethodCall (pe, (Method_id.of_string f), plst, lst) }
  | f = lname lst = function_call_params COLONCOLON plst = gen_ty_consume
    { Call ((Fn_id.of_string f), plst, lst) }

primary_expression_start:
  | NEW n = uname lst = function_call_params 
    { New ((Ty_id.of_string n), [], lst) }
  | NEW n = uname lst2 = function_call_params COLONCOLON lst1 = gen_ty_consume
    { New ((Ty_id.of_string n), lst1, lst2) }
  | LPAREN e = plain_expression RPAREN
    { e }
  | LBRACK RBRACK
    { Array [] }
  | LBRACK lst = separated_nonempty_list(COMMA, arithmetic_expression) RBRACK
    { Array lst }
  | i = literal
    { Literal i }
  | THIS
    { This }
  | n = name
    { Id ((Var_id.of_string n), [])}
  | n = name COLONCOLON plst = gen_ty_consume
    { Id ((Var_id.of_string n), plst) }

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
  | IF e1 = bin_expression COLON b1 = block lst = list(elif_expression) els = option(else_expression) END
    { let els_lst =
        match els with
        | Some e -> [e]
        | None -> [] in
      If (Cond (e1, b1) :: lst @ els_lst) }
elif_expression:
  | ELIF e = bin_expression COLON b = block
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
  | d = var_decorator n = lname COLON ty = ty
    { Var ((Var_id.of_string n), d, Some ty, None) }
  | d = var_decorator n = lname COLON ty = ty ASSIGN exp = arithmetic_expression
    { Var ((Var_id.of_string n), d, Some ty, Some exp) }
  | d = var_decorator n = lname ASSIGN exp = arithmetic_expression
    { Var ((Var_id.of_string n), d, None, Some exp) }
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
  | FN n = lname lst = function_params ARROW b = block END
    { Fn ((Fn_id.of_string n), [], lst, None, b) }
  | FN n = lname lst = function_params COLON t = ty ARROW b = block END
    { Fn ((Fn_id.of_string n), [], lst, Some t, b) }
  | FN n = lname lst1 = gen_ty_def lst2 = function_params ARROW b = block END
    { Fn ((Fn_id.of_string n), lst1, lst2, None, b) }
  | FN n = lname lst1 = gen_ty_def lst2 = function_params COLON t = ty ARROW b = block END
    { Fn ((Fn_id.of_string n), lst1, lst2, Some t, b) }

function_params:
  | LPAREN RPAREN
  { [] }
  | LPAREN lst = function_param_list RPAREN
  { lst }

function_param_list:
  | p = param_def { [p;] }
  | p = param_def COMMA lst = function_param_list { p :: lst }

param_def:
  | d = var_decorator n = lname COLON t = ty
    { Param ((Var_id.of_string n), d, Some t, None) }
  | d = var_decorator n = lname
    { Param ((Var_id.of_string n), d, None, None) }
  | d = var_decorator n = lname COLON t = ty ASSIGN exp = bin_expression
    { Param ((Var_id.of_string n), d, Some t, Some(exp)) }
  | d = var_decorator n = lname ASSIGN exp = bin_expression
    { Param ((Var_id.of_string n), d, None, Some(exp)) }

function_call_params:
  | LPAREN RPAREN { [] }
  | LPAREN lst = param_call_list RPAREN { lst }
param_call_list:
  | c = param_call { [c;] }
  | c = param_call COMMA lst = param_call_list { c :: lst }
param_call:
  | v = bin_expression
    { CallParam (None, v) }
  | n = lname ASSIGN v = bin_expression
    { CallParam (Some(Var_id.of_string n), v) }

block:
  | { Block [] }
  | e = expression { Block [e;] }
  | e = expression sep b = block { match b with  Block lst -> Block (e :: lst) | _ -> Block [e;] }

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
    { TyGen t }

tyname: t = name { Ty_id.of_string t }
genname: t = GNAME { Gen_id.of_string t }

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
  | n = uname f = class_father?
    { ((Ty_id.of_string n), [], f) }
  | n = uname lst = gen_ty_def f = class_father?
    { ((Ty_id.of_string n), lst, f) }
class_father: 
  | LPAREN f = uname RPAREN { ((Ty_id.of_string f), []) }
  | LPAREN f = uname lst = gen_ty_consume RPAREN { ((Ty_id.of_string f), lst) }
class_body:
  | { [] }
  | d = field_decl { [d;] }
  | d = field_decl sep lst = class_body { d :: lst }

trait_def: mark_position(plain_trait_def) { $1 }
plain_trait_def:
  | TRAIT n = uname lst1 = gen_ty_def f = trait_father? COLON fs = class_body END
    { Trait ((Ty_id.of_string n), f, lst1, fs) }
  | TRAIT n = uname f = trait_father? COLON fs = class_body END
    { Trait ((Ty_id.of_string n), f, [], fs) }
trait_father: 
  | LPAREN f = uname RPAREN { ((Ty_id.of_string f), []) }
  | LPAREN f = uname lst = gen_ty_consume RPAREN { ((Ty_id.of_string f), lst) }

field_decl: mark_position(plain_field_decl) { $1 }
plain_field_decl:
  | lst = separated_nonempty_list(COMMA, properties_decl)
    { Properties lst }
  | m = method_decl
    { m }

method_decl:
  | d = field_decorator FN n = lname lst = function_params ARROW b = block END
    { Method ((Method_id.of_string n), d, [], lst, None, b) }
  | d = field_decorator FN n = lname lst = function_params COLON t = ty ARROW b = block END
    { Method ((Method_id.of_string n), d, [], lst, Some t, b) }
  | d = field_decorator FN n = lname lst1 = gen_ty_def lst2 = function_params ARROW b = block END
    { Method ((Method_id.of_string n), d, lst1, lst2, None, b) }
  | d = field_decorator FN n = lname lst1 = gen_ty_def lst2 = function_params COLON t = ty ARROW b = block END
    { Method ((Method_id.of_string n), d, lst1, lst2, Some t, b) }

properties_decl:
  | anns = field_decorator n = lname COLON t = ty
    { Property ((Property_id.of_string n), anns, Some t, None) }
  | anns = field_decorator n = lname COLON t = ty ASSIGN exp = arithmetic_expression
    { Property ((Property_id.of_string n), anns, Some t, Some exp) }
  | anns = field_decorator n = lname ASSIGN exp = arithmetic_expression
    { Property ((Property_id.of_string n), anns, None, Some exp) }
field_decorator:
  | s = STATIC? p = PUB? c = CONST?
    {
      let jg = function
        | None -> false
        | Some _ -> true
      in { static= jg s; pub= jg p; const= jg c; }
    }

impl_def: mark_position(plain_impl_def) { $1 }
plain_impl_def:
  | IMPL c = uname FOR t = uname COLON fs = class_body END
    { Impl ((Ty_id.of_string c), (Ty_id.of_string t), [], [], fs) }
  | IMPL c = uname gs = gen_ty_def FOR t = uname COLON fs = class_body END
    { Impl ((Ty_id.of_string c), (Ty_id.of_string t), gs, [], fs) }
  | IMPL c = uname FOR t = uname gcs = gen_ty_consume COLON fs = class_body END
    { Impl ((Ty_id.of_string c), (Ty_id.of_string t), [], gcs, fs) }
  | IMPL c = uname gs = gen_ty_def FOR t = uname gcs = gen_ty_consume COLON fs = class_body END
    { Impl ((Ty_id.of_string c), (Ty_id.of_string t), gs, gcs, fs) }

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
