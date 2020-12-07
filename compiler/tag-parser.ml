#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)
let tag_parse_expressions sexps = raise X_syntax_error;;

  
(* get an sexpr and return its expr Sexpr -> Expr *)  

let rec tag_parse_expr = function
    (* Const *)
  | Number(sexpr) -> Const(Sexpr(Number(sexpr)))
  | Bool(sexpr) -> Const(Sexpr(Bool(sexpr)))
  | String(sexpr) -> Const(Sexpr(String(sexpr)))
  | Char(sexpr) -> Const(Sexpr(Char(sexpr)))
  | Pair(Symbol("quote"),Pair(sexpr,Nil)) -> Const(Sexpr(sexpr))
    (* Var *)
  | Symbol(sexpr) when (TAG_PARSER.no_reserved_word_exp sexpr) -> Var(sexpr)
    (* set! *)
  | Pair(Symbol("set!"), Pair(var_sexpr, Pair(val_sexpr, Nil))) -> Set(tag_parse_expr var_sexpr, tag_parse_expr val_sexpr)  
    (* pset! *)
  | Pair(Symbol("pset!"), pset_bang_body) -> pset_bang_macro_expns pset_bang_body
    (* If *)
  | Pair (Symbol("if"), Pair(test, Pair(thenExpr, Nil))) ->
    If(tag_parse_expr test, tag_parse_expr thenExpr, Const(Void))
  | Pair(Symbol("if"), Pair(test, Pair(thenExpr, Pair(elseExpr, Nil)))) ->
    If(tag_parse_expr test, tag_parse_expr thenExpr, tag_parse_expr elseExpr)
    (* Or *)
  | Pair(Symbol("or"),Nil) -> Const(Sexpr(Bool(false)))
  | Pair(Symbol("or"),Pair(sexpr,Nil)) -> tag_parse_expr sexpr
  | Pair(Symbol("or"),sexpr_list) -> Or(List.map tag_parse_expr (pair_to_list sexpr_list))
    (* Seq *)
  | Pair(Symbol("begin"),Nil) -> Const(Void)
  | Pair(Symbol("begin"),Pair(car,Nil)) -> tag_parse_expr car
  | Pair(Symbol("begin"),sexpr_list) -> Seq(List.flatten (List.map seq_expr (List.map tag_parse_expr (pair_to_list sexpr_list))))
    (* define *)
  | Pair(Symbol("define"),defineExpr) -> define_expr defineExpr
    (* Let *)
  | Pair(Symbol("let"), args_and_body_pair) -> let_macro_expns args_and_body_pair
    (* Let* *)
  |  Pair(Symbol("let*"), args_and_body_pair) -> tag_parse_expr (let_star_macro_expns args_and_body_pair)
  | Pair(Symbol("letrec"), args_and_body_pair) -> tag_parse_expr (letrec_macro_expns args_and_body_pair)
    (* Lambda *)
  | Pair(Symbol("lambda"), Pair(args_pair, body_pair)) -> 
      (try (lambda_simple_expr args_pair body_pair) 
      with X_syntax_error -> (lambda_opt_expr args_pair body_pair))
    (* Macro Expansions *)
    (* quasiquote *)
  | Pair(Symbol("quasiquote"),Pair(sexpr,Nil)) -> tag_parse_expr (quasiquote_expr sexpr)
    (* cond *)
  | Pair(Symbol("cond"),ribs) -> cond_expr ribs
    (* and *)
  | Pair(Symbol("and"),sexprs) -> and_expr sexprs
    (* Application *)
  | Pair(opt, args) -> 
    let opt_expr = (tag_parse_expr opt)
    and args_expr = (
          match args with 
          | Nil -> [] 
          | _ -> List.map tag_parse_expr (pair_to_list args)) in
    Applic(opt_expr, args_expr)
  | _ -> raise X_syntax_error;;

(* check if a word is reserved. Symbol -> Boolean*)

let no_reserved_word_exp sym = not (List.mem sym reserved_word_list);;

(* get a Pair and return a list Pair sexpr -> list sexpr *)

let rec pair_to_list pairs =
  match pairs with
  | Pair(sexpr, Nil) -> [sexpr]
  | Pair(car, cdr) -> car :: (pair_to_list cdr)
  | _ -> raise X_syntax_error;; 

let rec im_pair_to_list pairs =
  match pairs with
  | Symbol(s) -> [Symbol(s)]
  | Pair(car, cdr) -> car :: (im_pair_to_list cdr)
  | _ -> raise X_syntax_error;;  
  
let rec list_to_pair ls = 
  match ls with 
  | [] -> Pair(Nil, Nil)
  | [x] -> Pair(x, Nil) 
  | x :: y -> Pair(x, list_to_pair y);;

let rec get_last_item_on_list lst =
  match lst with 
  | [x] -> x
  | _ -> (get_last_item_on_list (List.tl lst));;

let rec get_list_besides_last_item lst =
  match lst with 
  | [x] -> []
  | [x; y] -> [x]
  | _ -> (List.hd lst) :: get_list_besides_last_item (List.tl lst);;

let sym_expr_to_string sym_expr =
  match sym_expr with 
  | Symbol(s) -> s
  | _ -> raise X_syntax_error;;

let get_let_var_from_pair pairs = 
  match pairs with 
  | Pair(var, value) -> var 
  | _ -> raise X_syntax_error;;

let get_var_from_let_binding binding = 
  match binding with 
  | Pair(var, Pair(value, rest)) -> var
  | Pair(var, value) -> var
  | _ -> raise X_syntax_error;;

let get_val_from_let_binding binding = 
  match binding with 
  | Pair(var, Pair(value, rest)) -> value
  | Pair(var, value) -> value
  | _ -> raise X_syntax_error;;

let rec flatten_list list_of_exprs = 
  match list_of_exprs with 
  | [] -> []
  | [Seq(x)] -> x
  | [x] -> [x]
  | x :: y -> List.append (flatten_list [x]) (flatten_list y);;
  
let rec get_list_of_aux_vars max_count init_num =
  if(init_num >= max_count)
  then []
  else 
    let ls = (Reader.read_sexprs ("ph" ^ (string_of_int init_num))) in
    ls @ (get_list_of_aux_vars max_count (init_num + 1));;

let rec get_list_of_sets_pair list_of_vars max_count init_num =
  if(init_num >= max_count)
  then []
  else 
    let ls = Reader.read_sexprs ("(set! ph" ^ (string_of_int init_num) ^ " " ^ (List.hd list_of_vars) ^ ")") in
    ls @ (get_list_of_sets_pair (List.tl list_of_vars) max_count (init_num + 1));;

let rec create_pairs_list_from_two_lists lst1 lst2 =
  match (lst1, lst2) with 
  | ([], []) -> []
  | (x1 :: x, y2 :: y) -> Pair(x1, Pair(y2, Nil)) :: (create_pairs_list_from_two_lists x y)
  | _ -> raise X_syntax_error

(* take care  of begin expr -> expr list *)
let rec seq_expr expr = 
match expr with
| Seq(sexpr_list) -> (List.flatten (List.map seq_expr sexpr_list))
| expr -> [expr];;

(* take care Macro Expansions of quasiquote Sexpr -> Sexpr  *)
let rec quasiquote_expr sexpr =
match sexpr with
| Nil -> Pair(Symbol("quote"),Pair(Nil,Nil))
| Pair(Symbol("unquote"),Pair(sexpr,Nil)) -> sexpr
| Pair(Symbol("unquote-splicing"),Pair(sexpr,Nil)) -> raise X_syntax_error
| Symbol(sym) -> Pair(Symbol ("quote"), Pair(Symbol(sym), Nil))
| Pair(Pair(Symbol ("unquote-splicing"),Pair(sexpr , Nil)),b) ->
  Pair(Symbol("append"),Pair(sexpr ,Pair((quasiquote_expr b),Nil)))
| Pair(a,Pair(Symbol ("unquote-splicing"),Pair(sexpr,Nil)))->
  Pair(Symbol("cons"),Pair(quasiquote_expr a,Pair(sexpr,Nil)))
| Pair(car, cdr) -> Pair(Symbol "cons", Pair(quasiquote_expr car, Pair(quasiquote_expr cdr, Nil)))
|_ ->  sexpr;;

(* take care of define Sexpr -> expr  *)
let define_expr defineExpr =
match defineExpr with
| Pair(Symbol(varName), Pair(value, Nil)) -> Def((tag_parse_expr (Symbol(varName))) , (tag_parse_expr value))
| Pair(Pair(Symbol(varName), argsList), exprList) ->
  tag_parse_expr (Pair(Symbol "define", Pair(Symbol(varName), Pair(Pair(Symbol "lambda", Pair(argsList, exprList)), Nil))))
| _-> raise X_syntax_error;;

(* take care Macro Expansions of cond Sexpr -> expr  *)
let rec cond_expr ribs = 
match ribs with
| Pair(Pair(cond, Pair(Symbol("=>"), exprf)), Nil) ->
  tag_parse_expr (Pair(Symbol("let"), Pair(Pair(Pair(Symbol("value"), 
  Pair(cond, Nil)), Pair(Pair(Symbol("f"), Pair(Pair(Symbol("lambda"),
  Pair(Nil, exprf)), Nil)), Nil)), Pair(Pair(Symbol("if"), Pair(Symbol("value"), 
  Pair(Pair(Pair(Symbol("f"), Nil), Pair(Symbol("value"), Nil)), Nil))), Nil))))
| Pair(Pair(cond, Pair(Symbol("=>"), exprf)), rest_of_ribs) ->
  tag_parse_expr (Pair(Symbol("let"), Pair(Pair(Pair(Symbol("value"), Pair(cond, Nil)),
  Pair(Pair(Symbol("f"), Pair(Pair(Symbol("lambda"), Pair(Nil, exprf)), Nil)),
  Pair(Pair(Symbol("rest"), Pair(Pair(Symbol("lambda"),
  Pair(Nil, Pair(Pair(Symbol("cond"), rest_of_ribs), Nil))), Nil)
  ), Nil))), Pair(Pair(Symbol("if"), Pair(Symbol("value"),
  Pair(Pair(Pair(Symbol("f"), Nil), Pair(Symbol("value"), Nil)), Pair(Pair(Symbol("rest"), Nil), Nil)))), Nil))))
| Pair(Pair(Symbol("else"),expr_list),_) -> tag_parse_expr (Pair(Symbol("begin"), expr_list))
| Pair(Pair(cond, rib1), Nil) ->  If(tag_parse_expr cond, tag_parse_expr (Pair(Symbol("begin"), rib1)), Const(Void)) 
| Pair(Pair(cond, rib1), rest_of_ribs) ->   If(tag_parse_expr cond, tag_parse_expr (Pair(Symbol("begin"), rib1)), cond_expr rest_of_ribs) 
| _ -> raise X_syntax_error;;

let lambda_simple_expr args_sexprs body_sexprs =    
let args_exprs = 
  (match args_sexprs with 
  | Nil -> []                     
  | _ -> List.map sym_expr_to_string (pair_to_list args_sexprs))  
and body_exprs = 
  (match body_sexprs with
  | Pair(first, Nil) -> tag_parse_expr first
  | _ -> Seq(flatten_list (List.map tag_parse_expr (pair_to_list body_sexprs)))) in
LambdaSimple(args_exprs, body_exprs);;

let lambda_opt_expr args_sexprs body_sexprs = 
let args_sexprs = 
  (match args_sexprs with 
  | Nil -> []                     
  | _ -> im_pair_to_list args_sexprs) in
let args_exprs = List.map sym_expr_to_string (get_list_besides_last_item args_sexprs)
and opt_expr = sym_expr_to_string (get_last_item_on_list args_sexprs)
and body_exprs = 
  (match body_sexprs with
  | Pair(first, Nil) -> tag_parse_expr first
  | _ -> Seq(flatten_list (List.map tag_parse_expr (pair_to_list body_sexprs)))) in
LambdaOpt(args_exprs, opt_expr, body_exprs);;

(* make and with nested if Sexpr -> Expr  *)
let rec and_expr sexprs = 
match sexprs with
| Nil -> Const(Sexpr(Bool(true)))
| Pair(car, Nil) -> tag_parse_expr car
| Pair(car, cdr) -> If((tag_parse_expr car) ,(and_expr cdr) ,Const(Sexpr(Bool(false))))
| _ -> raise X_syntax_error;;

let let_macro_expns sexprs = 
match sexprs with 
| Pair(args_pair, body_pair) -> 
  let args = 
    (match args_pair with 
    | Nil -> []
    | _ -> 
      List.map sym_expr_to_string (List.map get_var_from_let_binding (pair_to_list args_pair)))
  and body = 
    (match body_pair with
    | Pair(body_sexprs, Nil) -> tag_parse_expr body_sexprs
    | _ -> Seq(flatten_list (List.map tag_parse_expr (pair_to_list body_pair))))
  and vals = (match args_pair with 
    | Nil -> []
    | _ ->  flatten_list (List.map tag_parse_expr (List.map get_val_from_let_binding (pair_to_list args_pair)))) in
  Applic(LambdaSimple(args, body), vals)
| _ -> raise X_syntax_error;;

let rec let_star_macro_expns sexprs = 
match sexprs with 
| Pair(args_pair_sexpr, body_sexprs) -> 
    (match args_pair_sexpr with 
    | Nil -> Pair(Symbol("let"), Pair(Nil, body_sexprs))
    | Pair(first_binding, Nil) -> Pair(Symbol("let"), Pair(args_pair_sexpr, body_sexprs))
    | Pair(first_binding, rest) -> Pair(Symbol("let"), Pair(Pair(first_binding, Nil), Pair((let_star_macro_expns (Pair(rest, body_sexprs)), Nil))))
    | _ -> raise X_syntax_error)
| _ -> raise X_syntax_error;;

let letrec_macro_expns sexprs = 
match sexprs with 
| Pair(args, body) ->
  let place_holder = Pair(Symbol("quote"),Pair(Symbol("whatever"),Nil))
  and args = (pair_to_list args) in
  let vars_list = List.map get_var_from_let_binding args in
  let new_args_pair = list_to_pair (List.map (fun (x) -> Pair(x, Pair(place_holder, Nil))) vars_list) 
  and vals_list = (pair_to_list body)
  and new_body_list = List.map (fun (x) -> Pair(Symbol("set!"), x)) args in
  Pair(Symbol("let"), Pair(new_args_pair, list_to_pair (List.append new_body_list vals_list)))
| _ -> raise X_syntax_error;;
    
let pset_bang_macro_expns sexprs =     
let list_of_binding_pair = pair_to_list sexprs in
let max_count = (List.length list_of_binding_pair) in
  if(max_count == 1)
  then 
    (match sexprs with 
    | Pair(x, Nil) -> tag_parse_expr (Pair(Symbol("set!"), x))
    | _ -> raise X_syntax_error)
  else 
  let list_of_binding_vals = List.map get_val_from_let_binding list_of_binding_pair 
  and list_of_binding_vars = List.map get_var_from_let_binding list_of_binding_pair 
  and list_of_aux_vars = get_list_of_aux_vars max_count 0 in
  let list_of_aux_vars_and_vals_pairs = create_pairs_list_from_two_lists list_of_aux_vars list_of_binding_vals 
  and list_of_sets = list_to_pair (List.map (fun (x) -> Pair(Symbol("set!"), x)) (create_pairs_list_from_two_lists list_of_binding_vars list_of_aux_vars)) in
  let let_args = list_to_pair list_of_aux_vars_and_vals_pairs in
  tag_parse_expr (Pair(Symbol("let"), Pair(let_args, list_of_sets)));;
  
end;; (* struct Tag_Parser *)

