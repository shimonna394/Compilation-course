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
    | Box'(VarFree v1), Box'(VarFree v2) -> String.equal v1 v2
    | Box'(VarParam (v1,mn1)), Box'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
    | Box'(VarBound (v1,mj1,mn1)), Box'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
    | BoxGet'(VarFree v1), BoxGet'(VarFree v2) -> String.equal v1 v2
    | BoxGet'(VarParam (v1,mn1)), BoxGet'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
    | BoxGet'(VarBound (v1,mj1,mn1)), BoxGet'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
    | BoxSet'(VarFree v1,e1), BoxSet'(VarFree v2, e2) -> String.equal v1 v2 && (expr'_eq e1 e2)
    | BoxSet'(VarParam (v1,mn1), e1), BoxSet'(VarParam (v2,mn2),e2) -> String.equal v1 v2 && mn1 = mn2 && (expr'_eq e1 e2)
    | BoxSet'(VarBound (v1,mj1,mn1),e1), BoxSet'(VarBound (v2,mj2,mn2),e2) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2 && (expr'_eq e1 e2)
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
    if(d + 1 == depth) then VarParam(str, i) else VarBound(str, depth - d - 2, i)
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
     (demonstrate_tail_calls in_tail_p else_expr))
   | Seq'(expr_list) -> Seq'(last_in_tail expr_list)
   | Def'(var,exp) -> Def'(var, (demonstrate_tail_calls false exp)) 
   | Set'(var,exp) -> Set'(var, (demonstrate_tail_calls false exp)) 
   | Or'(expr_list) -> Or'(last_in_tail expr_list)
   | LambdaSimple'(args,body) -> LambdaSimple'(args,(demonstrate_tail_calls true body))
   | LambdaOpt'(args,opt,body) -> LambdaOpt'(args,opt,(demonstrate_tail_calls true body))
   | Applic'(proc,args) -> (if in_tail_p
     then ApplicTP'((demonstrate_tail_calls false proc), (List.map (demonstrate_tail_calls false) args))
     else Applic'((demonstrate_tail_calls false proc), (List.map (demonstrate_tail_calls false) args)))
   | ApplicTP'(proc,args) -> ApplicTP'((demonstrate_tail_calls false proc), (List.map (demonstrate_tail_calls false) args));
   
  and last_in_tail expr_list = 
    let reverse_list = (List.rev expr_list) in
    let last_expr = (List.hd reverse_list) in
    let rest_expr_list = (List.rev (List.tl reverse_list)) in
    (List.append (List.map (demonstrate_tail_calls false) rest_expr_list) [demonstrate_tail_calls true last_expr]);;

    (* box var to BoxGet and set to SetBox *)
  let rec add_boxing_param param body = 
      (match body with
         | Const'(expr) -> Const'(expr)
         | Var'(var) -> (match var with
                          | VarParam(name, _) when name = param -> BoxGet'(var)
                          | VarBound(name, _, _) when name = param -> BoxGet'(var)
                          | _ -> Var'(var))
         | Box'(var) -> Box'(var)
         | BoxGet'(var) -> BoxGet'(var)
         | BoxSet'(var, expr) -> BoxSet'(var, (add_boxing_param param expr))
         | If'(test, then_expr, else_expr) -> If'((add_boxing_param param test), (add_boxing_param param then_expr), (add_boxing_param param else_expr))
         | Seq'(expr_list) -> Seq'(List.map (add_boxing_param param) expr_list)
         | Set'(var_name, val_name) -> (match var_name with
                                      | VarParam(name, minor) when name = param -> BoxSet'(VarParam(name, minor), (add_boxing_param param val_name))
                                      | VarBound(name, major, minor) when name = param -> BoxSet'(VarBound(name, major, minor), (add_boxing_param param val_name))
                                      | _ -> Set'(var_name, (add_boxing_param param val_name)))
         | Def'(var_name, val_name) -> Def'(var_name, (add_boxing_param param val_name))
         | Or'(expr_list) -> Or'(List.map (add_boxing_param param) expr_list)
         | LambdaSimple'(args, body) -> (if (List.mem param args)
                                           then LambdaSimple'(args, body)
                                           else LambdaSimple'(args, (add_boxing_param param body)))
         | LambdaOpt'(args, opt, body) -> (if (List.mem param (List.append args [opt]))
                                           then LambdaOpt'(args, opt, body)
                                           else LambdaOpt'(args, opt, (add_boxing_param param body)))
         | Applic'(proc, args) -> Applic'((add_boxing_param param proc), (List.map (add_boxing_param param) args))
         | ApplicTP'(proc, args) -> ApplicTP'((add_boxing_param param proc), (List.map (add_boxing_param param) args)));
      
  and add_boxing  param body index = 
    (add_boxing_param param body);;

    (* the main function of boxing *)
let rec dem_boxing expr = 
  match expr with
   | Const'(expr) -> Const'(expr)
   | Var'(expr) -> Var'(expr)
   | Box'(expr) -> Box'(expr)
   | BoxGet'(expr) -> BoxGet'(expr)
   | BoxSet'(var,exp) -> BoxSet'(var, (dem_boxing exp))
   | If'(test,then_expr,else_expr) -> If'((dem_boxing test),
     (dem_boxing then_expr),
     (dem_boxing else_expr))
   | Seq'(expr_list) -> Seq'(List.map dem_boxing expr_list)
   | Def'(var,exp) -> Def'(var, (dem_boxing exp)) 
   | Set'(var,exp) -> Set'(var, (dem_boxing exp)) 
   | Or'(expr_list) -> Or'(List.map dem_boxing expr_list)
   | LambdaSimple'(args,body) -> LambdaSimple'(args, (dem_boxing(handle_box_lambda (List.rev args) body ((List.length args)-1))))
   | LambdaOpt'(args,opt,body) -> LambdaOpt'(args, opt, (dem_boxing(handle_box_lambda (List.rev(List.append args [opt])) body ((List.length args)))))
   | Applic'(proc,args) -> Applic'((dem_boxing proc), (List.map dem_boxing args))
   | ApplicTP'(proc,args) -> ApplicTP'((dem_boxing proc), (List.map dem_boxing args));

   (* handle lambda case - check we need boxing, if yes box the expr*)
and handle_box_lambda args body index =
 match args with
   | car :: cdr -> (if (check_boxing car body)
   then (let boxed_body1 = (add_boxing car body index) in
    let boxed_body = (match boxed_body1 with
       | Seq'(Set'((VarParam(name1, index1)), Box'(VarParam(name2, index2))) :: expr_list) when (name1 = name2) ->
         (Seq'(List.append [Set'(VarParam(car, index), Box'(VarParam(car, index)));
                           Set'(VarParam(name1, index1), Box'(VarParam(name2, index2)))] expr_list)) 
       | Seq'(expr_list) -> (Seq'(List.append [Set'(VarParam(car, index), Box'(VarParam(car, index)))] expr_list))
       | _ -> Seq'(List.append [Set'(VarParam(car, index), Box'(VarParam(car, index)))] [boxed_body1])) in 
   (handle_box_lambda cdr boxed_body  (index-1)))
   else (handle_box_lambda cdr body (index-1)))
   | [] -> body;

   (* check if the parameter in the expr should be boxed *)
and check_boxing param expr = 
    let read_occurs = ref [] in
    let write_occurs = ref [] in
    let ribs = ref 0 in
    let add_ribs num = 
      ribs := !ribs+1 in
    let add_to_reads num = 
     (if (not (List.mem num !read_occurs))
     then read_occurs := List.append !read_occurs [num]) in
    let add_to_writes num = 
     (if (not (List.mem num !write_occurs))
     then write_occurs := List.append !write_occurs [num]) in
   
   let take_second e expr = 
     expr in  
   
   let rec check_params param num expr = 
     (match expr with
       | Const'(expr) -> Const'(expr)
       | Var'(var) -> (match var with
                       | VarBound(name, major, minor) when name = param -> (let add_reads = add_to_reads num in
                                                                           take_second add_reads (Var'(var)))
                       | VarParam(name, index) when name = param -> (let add_reads = add_to_reads num in
                                                                           take_second add_reads (Var'(var)))
                       | _ -> Var'(var))
       | Box'(var) -> Box'(var)
       | BoxGet'(var) -> BoxGet'(var)
       | BoxSet'(var, expr) -> BoxSet'(var, (check_params param num expr))
       | If'(test, then_expr, else_expr) -> If'((check_params param num test), (check_params param num then_expr), (check_params param num else_expr))
       | Seq'(expr_list) -> Seq'(List.map (check_params param num) expr_list)
       | Set'(var_name, val_name) -> (match var_name with
                                   | VarBound(name, major, minor) when name = param -> (let add_writes = add_to_writes num in
                                                                                             take_second add_writes (Set'(var_name, (check_params param num val_name))))
                                   | _ -> Set'(var_name, (check_params param num val_name)))
       | Def'(var_name, val_name) -> Def'(var_name, (check_params param num val_name))
       | Or'(expr_list) -> Or'(List.map (check_params param num) expr_list)
       | LambdaSimple'(args, body) -> (if (List.mem param args)
                                         then LambdaSimple'(args, body)
                                         else LambdaSimple'(args, (check_params param num body)))
       | LambdaOpt'(args, opt, body) -> (if (List.mem param (List.append args [opt]))
                                                 then LambdaOpt'(args, opt, body)
                                                 else LambdaOpt'(args, opt, (check_params param num body)))
       | Applic'(proc, args) -> Applic'((check_params param num proc), (List.map (check_params param num) args))
       | ApplicTP'(proc, args) -> ApplicTP'((check_params param num proc), (List.map (check_params param num) args))) in
       
   let new_rib expr' = 
     let next_num = 0 in
     let num1 = add_ribs next_num in 
     check_params param !ribs expr' in
   
   let start_rib = 0 in
   
   let rec main_check_box body = 
     (match body with
       | Const'(expr) -> Const'(expr)
       | Var'(var) -> check_params param start_rib (Var'(var))
       | Box'(var) -> Box'(var)
       | BoxGet'(var) -> BoxGet'(var)
       | BoxSet'(var, expr) -> BoxSet'(var, (main_check_box expr))
       | If'(test, then_expr, else_expr) -> If'((main_check_box test), (main_check_box then_expr), (main_check_box else_expr))
       | Seq'(expr_list) -> Seq'(List.map main_check_box expr_list)
       | Set'(var_name,val_name) -> (match var_name with
                                   | VarParam(name, _) when name = param -> (let add_writes = add_to_writes start_rib in
                                                                                   (Set'((var_name), (main_check_box val_name))))
                                   | _ -> Set'(var_name, (main_check_box val_name)))
       | Def'(var_name,val_name) -> Def'(var_name, (main_check_box val_name))
       | Or'(expr_list) -> Or'(List.map main_check_box expr_list)
       | LambdaSimple'(args, body) -> new_rib (LambdaSimple'(args, body))
       | LambdaOpt'(args, opt, body) -> new_rib (LambdaOpt'(args, opt, body))
       | Applic'(proc, args) -> Applic'((main_check_box proc), (List.map main_check_box args))
       | ApplicTP'(proc, args) -> ApplicTP'((main_check_box proc), (List.map main_check_box args))) in
   
     let new_box = main_check_box expr in 
     let check_writes num =
     let remaining = List.filter (fun number -> number != num) !write_occurs in
     ((List.length remaining) > 0) in
     let lst = List.map (check_writes) !read_occurs in
     List.fold_right (fun a b -> a || b) lst false;;





(* and check_cond1 param write_occur expr =
 match expr with 
  | Seq'(expr_list) -> (match expr_list with
   | car :: [] -> (if (write_occur)
     then (check_read param car) 
     else false)
   | car :: cdr -> (match car with 
     Set'(var_name,val_name) when var_name = param -> 
     (let check_rest = (List.map (check_read param) cdr) in
     if(List.mem true check_rest)
     then true
     else false)
   | _ -> false
   
 and check_read param expr = 
    
*)

 

let annotate_lexical_addresses e = dem_lex_env [] 0 e;; 

let annotate_tail_calls e = demonstrate_tail_calls false e;;

let box_set e = dem_boxing e;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)


