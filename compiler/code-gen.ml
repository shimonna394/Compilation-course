#use "semantic-analyser.ml";;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

  let primitive_names_to_labels =
    [
      (* Type queries  *)
      "boolean?", "boolean?"; "flonum?", "flonum?"; "rational?", "rational?";
      "pair?", "pair?"; "null?", "null?"; "char?", "char?"; "string?", "string?";
      "procedure?", "procedure?"; "symbol?", "symbol?";
      (* String procedures *)
      "string-length", "string_length"; "string-ref", "string_ref"; "string-set!", "string_set";
      "make-string", "make_string"; "symbol->string", "symbol_to_string";
      (* Type conversions *)
      "char->integer", "char_to_integer"; "integer->char", "integer_to_char"; "exact->inexact", "exact_to_inexact";
      (* Identity test *)
      "eq?", "eq?";
      (* Arithmetic ops *)
      "+", "add"; "*", "mul"; "/", "div"; "=", "eq"; "<", "lt";
      (* Additional rational numebr ops *)
      "numerator", "numerator"; "denominator", "denominator"; "gcd", "gcd";
      (* you can add yours here *)
      "car", "car"; "cdr", "cdr";"set-car!", "set_car";"set-cdr!", "set_cdr";
      "cons", "cons"; "apply", "apply"; 
    ] 

  let empty_func =
    fun x -> ();;
  
  let take_second x y = 
    y;;  
  
  (* const table *)
  
  let final_list = ref
    [(Void, (0,"db T_VOID"));
    (Sexpr(Nil),(1,"db T_NIL"));
    (Sexpr(Bool true),(2,"db T_BOOL, 1"));
    (Sexpr(Bool false),(4,"db T_BOOL, 0"))];;
  
    let temp_list = ref [];;
    
  let sexp_list = ref [];;
  
  let rec get_sexp_list expr' = 
    (match expr' with
    | Const'(Sexpr(sexpr)) -> (let append_sexp = sexp_list := List.append !sexp_list [sexpr] in
                         empty_func append_sexp)
    | Const'(Void) -> ()
    | Var'(var) -> ()
    | Box'(var) -> ()
    | BoxGet'(var) -> ()
    | BoxSet'(var, expr) -> (get_sexp_list expr)
    | If'(test, dit, dif) -> (empty_func (List.map get_sexp_list [test; dit; dif])) 
    | Seq'(expr_list) -> (empty_func (List.map get_sexp_list expr_list))
    | Set'(set_var, set_val) -> (get_sexp_list set_val)
    | Def'(def_var, def_val) -> (get_sexp_list def_val)
    | Or'(expr_list) -> (empty_func (List.map get_sexp_list expr_list))
    | LambdaSimple'(params, body) -> (empty_func (get_sexp_list body))
    | LambdaOpt'(params, opt_param, body) -> (empty_func (get_sexp_list body))
    | Applic'(proc, args) -> (let a = [get_sexp_list proc] in
                                    let b = List.map get_sexp_list args in
                                    empty_func [a; b])
    | ApplicTP'(proc, args) -> (let a = [get_sexp_list proc] in
                                      let b = List.map get_sexp_list args in
                                      empty_func [a; b]));;
  
    let flatten_sexp expr'_list = List.map get_sexp_list expr'_list;;
  
    let rec get_index_from_table const_table sexp = 
      (match const_table with
      | (Sexpr(sexpr), (offset, str)) :: cdr when (sexpr_eq sexpr sexp) -> offset
      | car :: cdr -> get_index_from_table cdr sexp
      | [] -> (-1));;
  
    let rec find_sexp sexp_list sexp = 
      (match sexp_list with
      | String(str) :: rest when (sexpr_eq (String(str)) sexp) -> true
      | car :: cdr -> find_sexp cdr sexp
      | [] -> false);;  

    let rec find_sexp2 sexp_list sexp = 
        (match sexp_list with
        | (Sexpr(sexpr), false) :: rest when (sexpr_eq sexpr sexp) -> true
        | car :: cdr -> find_sexp2 cdr sexp
        | [] -> false);;    
  
    let sexp_to_string sexp = (match sexp with
      | Number(Fraction(x,y)) -> "("^string_of_int(x)^","^string_of_int(y)^")"
      | Number(Float(num)) -> string_of_float num
      | Char(ch) -> string_of_int(int_of_char(ch))
      | String(str) -> str
      | Symbol(str) -> str
      | _ -> "");;
      
      let rec sexp_const_temp sexp = 
        (if (find_sexp2 !temp_list sexp) = false
        then (match sexp with
        | Number(Fraction(x,y)) -> (let x = temp_list := List.append !temp_list [(Sexpr(sexp), false)] in
          empty_func [x])
        | Number(Float(num)) -> (let x = temp_list := List.append !temp_list [(Sexpr(sexp), false)] in
          empty_func [x])
        | Char(ch) -> (let x = temp_list := List.append !temp_list [(Sexpr(sexp), false)] in
          empty_func [x])
        | String(str) -> (let x = temp_list := List.append !temp_list [(Sexpr(sexp), false)] in
          empty_func [x])
        | Symbol(str) -> 
          (if find_sexp !sexp_list (String(str)) = false
          then (let first_step = sexp_const_temp (String(str)) in
          let x = temp_list := List.append !temp_list [(Sexpr(sexp), false)] in
          empty_func [first_step, x])
          else (let x = temp_list := List.append !temp_list [(Sexpr(sexp), false)] in
          empty_func [x]))
        | Pair(x,y)-> (let car_pair = sexp_const_temp x in
          let cdr_pair = sexp_const_temp y in
          let z = temp_list:=List.append !temp_list [(Sexpr(sexp), false)] in
          empty_func [car_pair, cdr_pair,z])
        | _ -> ())
          else ());; 
    
    let rec make_temp_from_sexp_list sexp_list = 
      (match sexp_list with
      | [] -> []
      | car :: cdr -> 
        let first_sexp = sexp_const_temp car in
        take_second first_sexp (make_temp_from_sexp_list cdr));;
  
    let offset = ref 6;;
    
    let make_cosnt_sexp sexp = 
      (match sexp with
      | (Sexpr(Number(Fraction(x,y))),false) -> (let x = final_list := List.append !final_list [(Sexpr(Number(Fraction(x,y))),(!offset,"MAKE_LITERAL_RATIONAL "^sexp_to_string (Number(Fraction(x,y)))^""))] in
      let add_to_offset = offset := !offset + 17 in
      empty_func [x,add_to_offset])
      | (Sexpr(Number(Float(num))),false) -> (let x = final_list := List.append !final_list [(Sexpr(Number(Float(num))),(!offset,"MAKE_LITERAL_FLOAT "^sexp_to_string (Number(Float(num)))^""))] in
      let add_to_offset = offset := !offset + 9 in
      empty_func [x,add_to_offset])
      | (Sexpr(Char(ch)),false) -> (let x = final_list := List.append !final_list [(Sexpr(Char(ch)),(!offset,"MAKE_LITERAL_CHAR "^sexp_to_string (Char(ch))^""))] in
      let add_to_offset = offset := !offset + 2 in
      empty_func [x,add_to_offset])
      | (Sexpr(String(str)),false) -> (let x = final_list := List.append !final_list [(Sexpr(String(str)),(!offset,"MAKE_LITERAL_STRING \""^str^"\", "^string_of_int(String.length str)^""))] in
      let add_to_offset = offset := !offset + 9 + (String.length str) in
      empty_func [x,add_to_offset])
      | (Sexpr(Symbol(str)),false) -> (let x = final_list := List.append !final_list [(Sexpr(Symbol(str)),(!offset,"MAKE_LITERAL_SYMBOL const_tbl+ "^string_of_int (get_index_from_table !final_list (String(str)))^""))] in
      let add_to_offset = offset := !offset + 9 in
      empty_func [x,add_to_offset])
      | (Sexpr(Pair(x,y)),false) -> (let x = final_list := List.append !final_list [(Sexpr(Pair(x,y)),(!offset,"MAKE_LITERAL_PAIR(const_tbl+ "^string_of_int (get_index_from_table !final_list x)^", const_tbl+ "^string_of_int (get_index_from_table !final_list y)^")"))] in
      let add_to_offset = offset := !offset + 17 in
      empty_func [x,add_to_offset])
      | _ -> ());;
  
     let get_const_tables expr'_list = 
      let first_step = flatten_sexp expr'_list in
      let second_step = make_temp_from_sexp_list !sexp_list  in
      let third_step = List.map make_cosnt_sexp !temp_list in 
      let ans = List.append !final_list [] in
      let clean = final_list := [(Void, (0,"db T_VOID"));
      (Sexpr(Nil),(1,"db T_NIL"));
      (Sexpr(Bool true),(2,"db T_BOOL, 1"));
      (Sexpr(Bool false),(4,"db T_BOOL, 0"))] in
      let clean2 = sexp_list := [] in
      let clean3 = temp_list := [] in
      let clean4 = offset := 6 in
      let empty_all = empty_func [first_step, second_step, third_step, clean, clean2, clean3, clean4] in
      take_second empty_all ans;; 
  
       (* free vars table *)
       
  let rec append_if_not_exists_expended name acc table =
    match acc with 
    | [] -> table @ [(name, (List.length table))]
    | first :: rest -> 
      (match first with 
      | (other, _) when other = name -> table
      | (other, _) -> (append_if_not_exists_expended name rest table));;

  let append_if_not_exists name table = (append_if_not_exists_expended name table table);;

  let rec get_fvar_table_expended expr'_list table =
    match expr'_list with 
    | [] -> table
    | expr' :: rest -> 
      (match expr' with 
      | Def'(VarFree(name), _) -> 
          (get_fvar_table_expended rest (append_if_not_exists name table))
      | Applic'(Var'(VarFree(name)), _) -> (get_fvar_table_expended rest (append_if_not_exists name table))
      | ApplicTP'(Var'(VarFree(name)), _) -> (get_fvar_table_expended rest (append_if_not_exists name table))
      | Seq'(exprs') -> (get_fvar_table_expended exprs' table)
      | _ -> (get_fvar_table_expended rest table));;

  let rec defualt_fvar_table table plt = 
    match plt with 
    | [] -> table
    | first :: rest ->
      match first with 
      | (prim, label) -> defualt_fvar_table (table @ [(prim, List.length table)]) rest

  let get_fvar_table expr'_list = (get_fvar_table_expended expr'_list (defualt_fvar_table [] primitive_names_to_labels));;

  (* generate *)

  let counter = ref 0;;

  let get_counter num = 
    let add_count = counter := !counter+1 in
    let ans = !counter in
    take_second add_count ans;;

  let get_counter_s num = string_of_int (get_counter num);;

  let rec get_index_of_free_var name fvars = 
    match fvars with 
    | [] -> raise X_syntax_error
    | first :: rest -> 
      match first with 
      | (other, index) when (String.equal other name) -> (string_of_int index)
      | _ -> get_index_of_free_var name rest;;

  let rec main_generate const_tbl fvars depth expr' = 
  match expr' with
  | Const'(Sexpr(sexp)) -> "mov rax, const_tbl + "^string_of_int (get_index_from_table const_tbl sexp)^"\n"
  | Const'(Void) -> "mov rax, const_tbl + 0\n"
  | Box'(var) -> (main_generate const_tbl fvars depth (Var'(var)))^
    "push rax
     MALLOC rax, 8
     pop qword[rax]\n"
  | BoxGet'(var) -> (main_generate const_tbl fvars depth (Var'(var)))^
    "mov rax, qword[rax]\n" 
  | BoxSet'(var, expr) -> (main_generate const_tbl fvars depth expr)^
    "push rax\n"^
    (main_generate const_tbl fvars  depth (Var'(var)))^
    "pop qword[rax]\n"^
    "mov rax, SOB_VOID_ADDRESS\n"
  | Or'(expr_list) -> generate_or const_tbl fvars depth expr_list (get_counter_s 1)
  | If'(test, dit, dif) -> 
    let else_label = "Lelse"^ get_counter_s 1
    and exit_label = "Lexit" ^ get_counter_s 1 in
      (main_generate const_tbl fvars  depth test) ^ "\n" ^
      "cmp rax, SOB_FALSE_ADDRESS\n" ^
      "je " ^ else_label ^ "\n" ^
      (main_generate const_tbl fvars depth dit) ^ "\n" ^
      "jmp " ^ exit_label ^ "\n" ^
      else_label ^ ":\n" ^ (main_generate const_tbl fvars  depth dif) ^ "\n" ^
      exit_label^":\n"
  | Seq'(expr'_list) -> String.concat "\n" (List.map (main_generate const_tbl fvars depth) expr'_list)
  | Def'(VarFree(name), value) -> 
    (main_generate const_tbl fvars depth value) ^ "\n" ^
    "mov [fvar_tbl + " ^ (get_index_of_free_var name fvars) ^ " * 8], rax\n" ^
    "mov rax, SOB_VOID_ADDRESS\n"
  | Set'(VarFree(name), value) -> 
    (main_generate const_tbl fvars depth value) ^ "\n" ^
    "mov [fvar_tbl + " ^ (get_index_of_free_var name fvars) ^ " * 8], rax\n" ^
    "mov rax, SOB_VOID_ADDRESS\n"
  | Set'(VarParam(name, minor), value) ->  
    (main_generate const_tbl fvars depth value) ^
    "mov qword[rbp+8*(4+"^string_of_int(minor)^")], rax
     mov rax, SOB_VOID_ADDRESS\n"
  | Set'(VarBound(name, minor, major), value) ->
    (main_generate const_tbl fvars depth value) ^
    "mov rbx, qword[rbp+(8*2)]
     mov rbx, qword[rbx+(8*"^string_of_int(major)^")]
     mov qword[rbx+(8*"^string_of_int(minor)^")], rax
     mov rax, SOB_VOID_ADDRESS\n"
  | Var'(VarFree(name)) -> 
    "mov rax, [fvar_tbl + (" ^ (get_index_of_free_var name fvars) ^ " * 8)]\n"
  | Var'(VarParam(_,minor)) ->
    "mov rax, qword[rbp+ 8*(4+"^string_of_int(minor)^")]\n"
  | Var'(VarBound(_,major,minor)) ->
    "mov rax, qword[rbp+8*2]
     mov rax, qword[rax+(8*"^string_of_int(major)^")]
     mov rax, qword[rax+(8*"^string_of_int(minor)^")]\n"  
  | LambdaSimple'(args, body) -> generate_lambda_simple const_tbl fvars depth body
  | LambdaOpt'(args, opt, body) -> generate_lambda_opt const_tbl fvars depth (List.length args) body  
  | Applic'(op, exprs) -> 
  let index = get_counter_s 1 in
  (* Pushing evaluated args to stack *)
  "push SOB_NIL_ADDRESS ; push the magic\n"^
  (String.concat "\n" (List.rev_map (fun exp -> (main_generate const_tbl fvars depth exp) ^ "\npush rax\n") exprs)) ^
  "push "^string_of_int(List.length exprs)^"\n"^
  (* Evaluating op *)
  (main_generate const_tbl fvars depth op) ^ "\n" ^
  (* Getting the right closure from the fvar table *)
  "CLOSURE_ENV rbx, rax ; get the env from the closure
   push rbx ; push the env
   CLOSURE_CODE rax, rax ;  get the code
   call rax
   mov rdx, [rsp+8] ; get the number of arguments
   add rdx, 3 ; add the magic, num of args, env
   cleanloop"^index^": ; clean the stack
   cmp rdx, 0
   je end_cleanloop"^index^"
   add rsp, 8
   dec rdx
   jmp cleanloop"^index^"
   end_cleanloop"^index^":\n"
  | ApplicTP'(op, exprs) -> 
  let index = get_counter_s 1 in
 "push SOB_NIL_ADDRESS ; push the magic\n"^
  (String.concat "\n" (List.rev_map (fun exp -> (main_generate const_tbl fvars depth exp) ^ "\npush rax\n") exprs)) ^
  "push "^string_of_int(List.length exprs)^"\n"^
  (* Evaluating op *)
  (main_generate const_tbl fvars depth op) ^ "\n" ^
  (* Getting the right closure from the fvar table *)
   "CLOSURE_ENV rbx, rax ; get the env from the closure
   push rbx ; push the env
   CLOSURE_CODE rax, rax ;  get the code
   push qword[rbp+8] ; push the old ret address
   mov rcx, qword[rsp+16] ; rcx has the number of arguments of the new frame
   add rcx, 3 ; add the ret, env, num of args
   mov rdx, qword[rbp+24] ; rdx has the number of arguments of the old frame
   add rdx, 4 ; add the rbp, ret, env, num of args
   mov rbx, qword[rbp] ; save the old rbp
   copy_stack_tp"^index^": ; loop for fix the stack
   cmp rcx, 0
   je end_stack_tp"^index^"
   mov rdi, qword[rsp+((rcx)*8)]
   mov [rbp+((rdx)*8)], rdi
   dec rcx
   dec rdx
   jmp copy_stack_tp"^index^"
   end_stack_tp"^index^":
   mov rdi, qword[rsp+((rcx)*8)]
   mov [rbp+((rdx)*8)], rdi
   mov rsp, rbp
   shl rdx, 3
   add rsp, rdx ; cleaninng the stack
   mov rbp, rbx ; restore the old rbp
   jmp rax\n" 
  | _ -> raise X_not_yet_implemented;

  and generate_or const_tbl fvars depth expr_list index =
      (match expr_list with
    | [] -> "Lexit_or"^index^":\n"
    | expr :: [] -> (main_generate const_tbl fvars depth expr)^"Lexit_or"^index^":\n"
    | expr :: exprs -> (main_generate const_tbl fvars depth expr)^ 
    "cmp rax, SOB_FALSE_ADDRESS 
     jne Lexit_or"^index^"\n"^
    (generate_or const_tbl fvars depth exprs index));

  and make_ext_env depth index =
   (if depth = 0
   then "mov rbx, SOB_NIL_ADDRESS ; rbx hold the env which is empty in this situation\n"
   else "mov rcx, qword[rbp+8*3] ; rcx hold the number of the arguments in the stack
   lea rcx, [(rcx+1)*8] ; rcx hold now the number of bytes that sholuld be allocated for extenv[0]
   MALLOC rax, rcx ; rax hold the pointer to extenv[0]
   mov rcx, qword[rbp+8*3] ; rcx hold now the number of bytes that sholuld be allocated for extenv[0]
   mov rdx, 0 ; rdx is the index to iterate the arguments in the stack
   start_copy_loop"^index^":
   cmp rdx, rcx ; check if the loop should be finished
   je end_loop"^index^"
   mov rbx, [rbp+8*(4+rdx)] ; rbx hold the argumnet in the stack according to rdx index
   mov [rax+8*rdx], rbx ; put the argument from the stack in the extenv[0]
   inc rdx ; inc the index of rdx
   jmp  start_copy_loop"^index^"
   end_loop"^index^":
   mov qword[rax+8*rdx], SOB_NIL_ADDRESS ; adding the magic
   MALLOC rbx, "^string_of_int((depth+1)*8)^" ; allocate bytes for extenv
   mov [rbx], rax ; extenv[0] = rax
   mov rax, qword[rbp+8*2] ; rax = old env 
   mov rdx, 0 ; rdx = index for the loop
   mov rcx, "^string_of_int(depth)^" ; number of elements in old env
   start_env_loop"^index^":
   cmp rdx, rcx ; check if the loop should be finished
   je end_env_loop"^index^"
   mov rdi, [rax+rdx*8] ; rdi hold the pointer to oldenv[rdx]
   mov[rbx+8+(rdx*8)], rdi ; extenv[rdx+1]=oldenv[rdx]
   inc rdx ; inc the index of rdx
   jmp start_env_loop"^index^"
   end_env_loop"^index^":\n");  

  and generate_lambda_simple const_tbl fvars depth body = 
   let index = get_counter_s 1 in
   let ext_env = make_ext_env depth index in
   let l_code = "Lcode"^index^":
    push rbp
    mov rbp, rsp\n"^
    (main_generate const_tbl fvars (depth+1) body)^"
    leave
    ret
    Lcont"^index^":\n" in
    ext_env^
    "MAKE_CLOSURE(rax,rbx, Lcode"^index^")
    jmp Lcont"^index^"\n"^
    l_code;

  and generate_lambda_opt const_tbl fvars depth num_of_args body = 
   let index = get_counter_s 1 in
   let ext_env = make_ext_env depth index in
   let fix_stack = 
    "push rbp
     mov rbp, rsp
     mov rbx, qword[rbp+3*8] ; rbx hold the number of argumnents
     mov rcx, "^string_of_int(num_of_args)^" ; rcx hold the number of arguments that lambda opt has
     cmp rbx, rcx ; if they are equal, there is no need to fix the stack
     je finish_fix_stack"^index^"
     mov rdx, 0
     mov rdi, [rbp+((4+rbx-1)*8)] ; rdi hold the last parameter
     MAKE_PAIR(rax, rdi, SOB_NIL_ADDRESS) ; make pair from the last argument
     push rbx
     dec rbx
     start_pair_loop"^index^": ; making a list for the opt parameter
     cmp rbx, rcx
     je end_pair_loop"^index^"
     mov rdi, [rbp+((4+rbx-1)*8)] ; rdi hold the current parameter
     mov rsi, rax ; save the pointer for the current pair 
     MAKE_PAIR(rax, rdi, rsi) ; update the pair
     dec rbx
     inc rdx
     jmp start_pair_loop"^index^"
     end_pair_loop"^index^":
     pop rbx
     mov [rbp+((4+rbx-1)*8)], rax ; put the list for lambda opt parameter in the stack
     add rbx, 2
     mov rcx, rbx
     sub rcx, rdx
     start_copy_stack"^index^": ; loop for copy the stack
     mov rax, qword[rbp+rcx*8]
     mov [rbp+rbx*8], rax
     dec rbx
     dec rcx
     cmp rcx, 0
     jne start_copy_stack"^index^"
     end_copy_stack"^index^":
     mov rax, qword[rbp+rcx*8]
     mov [rbp+rbx*8], rax
     pop_loop"^index^": ; cleaning the stack
     cmp rdx, 0
     je end_pop_loop"^index^"
     add rsp, 8
     dec rdx
     jmp pop_loop"^index^"
     end_pop_loop"^index^":
     mov rbp, rsp
     mov qword[rbp+24], "^string_of_int(num_of_args+1)^" ; update the number of arguments in the stack
     finish_fix_stack"^index^":\n" in
     let label_lcode = "Lcode"^index^":\n" in
     let lcode_body = label_lcode^fix_stack^
     (main_generate const_tbl fvars (depth+1) body)^"
     leave
     ret
     Lcont"^index^":\n" in
     ext_env^
    "MAKE_CLOSURE(rax,rbx, Lcode"^index^")
     jmp Lcont"^index^"\n"^
     lcode_body;;

  let make_consts_tbl asts = get_const_tables asts;; 
  let make_fvars_tbl asts = get_fvar_table asts;;
  let generate consts fvars e = main_generate consts fvars 0 e;;
end;;

let gen e = String.concat "\n" (List.map (Code_Gen.generate (Code_Gen.make_consts_tbl e) (Code_Gen.make_fvars_tbl e)) e);;
