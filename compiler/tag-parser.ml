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



let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)

(* check if a word is reserved. Symbol -> Boolean*)

let no_reserved_word_exp sym = not (List.mem sym reserved_word_list);;

(* get a Pair and return a list Pair sexpr -> list sexpr *)

let rec pair_to_list pairs =
  match pairs with
  | Pair(sexpr, Nil) -> [sexpr]
  | Pair(car, cdr) -> car :: (pair_to_list cdr)
  | _ -> raise X_syntax_error;;

let rec pair_im_to_list pairs =
  match pairs with
  | Symbol(s) -> [Symbol(s)]
  | Pair(car, cdr) -> car :: (pair_to_list cdr)
  | _ -> raise X_syntax_error;;  

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

(* get an sexpr and return its expr Sexpr -> Expr *)  

let rec tag_parse_expr = function
    (* Const *)
  | Number(sexpr) -> Const(Sexpr(Number(sexpr)))
  | Bool(sexpr) -> Const(Sexpr(Bool(sexpr)))
  | String(sexpr) -> Const(Sexpr(String(sexpr)))
  | Char(sexpr) -> Const(Sexpr(Char(sexpr)))
  | Pair(Symbol("quote"),Pair(sexpr,Nil)) -> Const(Sexpr(sexpr))
    (* Var *)
  | Symbol(sexpr) when (no_reserved_word_exp sexpr) -> Var(sexpr)
    (* If *)
  | Pair (Symbol("if"), Pair(test, Pair(thenExpr, Nil))) ->
    If(tag_parse_expr test, tag_parse_expr thenExpr, Const(Void))
  | Pair(Symbol("if"), Pair(test, Pair(thenExpr, Pair(elseExpr, Nil)))) ->
    If(tag_parse_expr test, tag_parse_expr thenExpr, tag_parse_expr elseExpr)
    (* Or *)
  | Pair(Symbol("Or"),Nil) -> Const(Sexpr(Bool(false)))
  | Pair(Symbol("Or"),Pair(sexpr,Nil)) -> tag_parse_expr sexpr
  | Pair(Symbol("Or"),sexpr_list) -> Or(List.map tag_parse_expr (pair_to_list sexpr_list))
    (* Seq *)
  | Pair(Symbol("begin"),Nil) -> Const(Void)
  | Pair(Symbol("begin"),Pair(Symbol(sym),Nil)) -> Var(sym)
  | Pair(Symbol("begin"),sexpr_list) -> Seq(List.map tag_parse_expr (pair_to_list sexpr_list))
    (* Lambda *)   
  | Pair(Symbol("lambda"), Pair(args_sexprs, body_sexprs)) -> 
    (* lambdaSimple *)    
    (try 
      (let args_exprs = 
        (match args_sexprs with 
        | Nil -> []                     
        | _ -> List.map sym_expr_to_string (pair_to_list args_sexprs))  
      and body_exprs = 
        (match body_sexprs with 
        | Pair(first, Nil) ->  tag_parse_expr first
        | _ -> Seq(List.map tag_parse_expr (pair_to_list body_sexprs))) in
      LambdaSimple(args_exprs, body_exprs))
    with X_syntax_error -> 
    (* lambdaOpt *)    
      (let args = 
        (match args_sexprs with
        | Pair(x, y) -> (pair_im_to_list args_sexprs)
        | Symbol(s) -> [Symbol(s)]
        | _ -> raise X_syntax_error) in
      let args_exprs = (get_list_besides_last_item args)
      and opt_expr = (sym_expr_to_string (get_last_item_on_list args)) in
      let body_exprs = 
        (match body_sexprs with 
        | Pair(first, Nil) ->  tag_parse_expr first
        | _ -> Seq(List.map tag_parse_expr (pair_to_list body_sexprs))) in
      LambdaOpt((List.map sym_expr_to_string args_exprs), opt_expr, body_exprs)))
    (* Macro Expansions *)
    (* quasiquote *)
  | Pair(Symbol("quasiquote"),Pair(sexpr,Nil)) -> tag_parse_expr (quasiquote_expr sexpr)
    (* cond *)
  | Pair(Symbol("cond"),ribs) -> tag_parse_expr (cond_expr ribs)
    (* and *)
  | Pair(Symbol("and"),sexprs) -> and_expr sexprs;
  (* Application *)
  | Pair(opt, args) -> 
  let opt_expr = (tag_parse_expr opt)
  and args_expr = List.map tag_parse_expr (pair_to_list args) in
  Applic(opt_expr, args_expr)
  | _ -> raise X_syntax_error;

  (* take care Macro Expansions of quasiquote Sexpr -> Sexpr  *)
  and quasiquote_expr sexpr =
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
  |_ ->  sexpr;

  (* take care Macro Expansions of cond Sexpr -> Sexpr  *)
  and cond_expr ribs = raise X_not_yet_implemented;

  (* make and with nested if Sexpr -> Expr  *)
  and and_expr sexprs = 
    match sexprs with
  | Nil -> Const(Sexpr(Bool(true)))
  | Pair(car, Nil) -> tag_parse_expr car
  | Pair(car, cdr) -> If((tag_parse_expr car) ,(and_expr cdr) ,Const(Sexpr(Bool(false))))
  | _ -> raise X_syntax_error;

  (*and expand_let exprs =     
    let vars_list = List.map get_let_var_from_pair (pair_to_list exprs) in*)


module Tag_Parser : TAG_PARSER = struct

let tag_parse_expressions sexpr = List.map tag_parse_expr sexpr;;

  
end;; (* struct Tag_Parser *)

