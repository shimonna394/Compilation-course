#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
  
  let rec get_list_of_args_list count depth args =
    match args with 
    | [] -> []
    | hd :: tl -> [hd; string_of_int count; string_of_int depth] :: (get_list_of_args_list (count + 1) depth tl)
                       
exception X_syntax_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct

let rec get_list_of_args_list count depth args =
  match args with 
  | [] -> []
  | hd :: tl -> [hd; string_of_int count; string_of_int depth] :: (get_list_of_args_list (count + 1) depth tl)

let check_stack_and_return_var stack depth str = 
  match (List.filter (fun(pair) -> String.equal (List.hd pair) str) stack) with
  | [] -> VarFree(str)
  | [s; i; d] :: rest -> 
    let d = int_of_string d 
    and i = int_of_string i in
    if(d + 1 == depth) then VarParam(str, i) else VarBound(str, i, depth - d - 1)
  | _ -> raise X_syntax_error;;
    
let rec dem_lex_env stack depth expr = 
  match (expr) with 
  | Const(expr) -> Const'(expr)
  | If(test, dit, dif) -> If'(dem_lex_env stack depth test, dem_lex_env stack depth dit, dem_lex_env stack depth dif)
  | Or(exprs) -> Or'(List.map (dem_lex_env stack depth) exprs)
  | Seq(exprs) -> Seq'(List.map (dem_lex_env stack depth) exprs)  
  | LambdaSimple(args, body) -> 
    let new_stack = get_list_of_args_list 0 depth args in    
    LambdaSimple'(args, dem_lex_env (new_stack @ stack) (depth + 1) body)
  | LambdaOpt(args, opt, body) -> 
    let new_stack = get_list_of_args_list 0 depth (args @ [opt]) in    
    LambdaOpt'(args, opt, dem_lex_env (new_stack @ stack) (depth + 1) body)
  | Var(str) -> Var'(check_stack_and_return_var stack depth str)      
  | Set(Var(str), value) -> Set'(check_stack_and_return_var stack depth str, dem_lex_env stack depth value)
  | Def(Var(str), value) -> Def'(check_stack_and_return_var stack depth str, dem_lex_env stack depth value)
  | Applic(expr, exprs) -> Applic'(dem_lex_env stack depth expr, List.map (dem_lex_env stack depth) exprs)
  | _ -> raise X_not_yet_implemented;;

  let rec demonstrate_tail_calls in_tail_p expr =  
    match expr with
   | Const'(expr) -> Const'(expr)
   | Var'(expr) -> Var'(expr)
   | Box'(expr) -> Box'(expr)
   | BoxGet'(expr) -> BoxGet'(expr)
   | BoxSet'(var,exp) -> BoxSet'(var, (demonstrate_tail_calls in_tail_p exp))
   | If'(test,then_expr,else_expr) -> If'((demonstrate_tail_calls false test),
     (demonstrate_tail_calls in_tail_p then_expr),
     (demonstrate_tail_calls in_tail_p then_expr))
   | Seq'(expr_list) -> Seq'(last_in_tail expr_list)
   | Def'(var,exp) -> Def'(var, (demonstrate_tail_calls false exp)) 
   | Set'(var,exp) -> Set'(var, (demonstrate_tail_calls false exp)) 
   | Or'(expr_list) -> Or'(last_in_tail expr_list)
   | LambdaSimple'(args,body) -> LambdaSimple'(args,(demonstrate_tail_calls true body))
   | LambdaOpt'(args,opt,body) -> LambdaOpt'(args,opt,(demonstrate_tail_calls true body))
   | Applic'(proc,args) -> (match in_tail_p with
   | true -> ApplicTP'((demonstrate_tail_calls false proc), (List.map (demonstrate_tail_calls false) args))
   | _ -> Applic'((demonstrate_tail_calls false proc), (List.map (demonstrate_tail_calls false) args)))
   | ApplicTP'(proc,args) -> ApplicTP'((demonstrate_tail_calls false proc), (List.map (demonstrate_tail_calls false) args));
   
  and last_in_tail expr_list = 
    let reverse_list = (List.rev expr_list) in
    let last_expr = (List.hd reverse_list) in
    let rest_expr_list = (List.rev (List.tl reverse_list)) in
    (List.append (List.map (demonstrate_tail_calls false) rest_expr_list) [demonstrate_tail_calls true last_expr]);;  

let annotate_lexical_addresses e = dem_lex_env [] 0 e;; 

let annotate_tail_calls e = demonstrate_tail_calls false e;;

let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)


