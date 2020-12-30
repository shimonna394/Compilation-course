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

let empty_func =
  fun x -> ();;

let take_second x y = 
  y;;  

(* const table *)

let final_list = ref
  [(Void, (0,"MAKE_VOID"));
  (Sexpr(Nil),(1,"MAKE_NIL"));
  (Sexpr(Bool false),(2,"MAKE_BOOL(0)"));
  (Sexpr(Bool true),(4,"MAKE_BOOL(1)"))];;

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
  | Seq'(exprs) -> (empty_func (List.map get_sexp_list exprs))
  | Set'(set_var, set_val) -> (get_sexp_list set_val)
  | Def'(def_var, def_val) -> (get_sexp_list def_val)
  | Or'(exprs) -> (empty_func (List.map get_sexp_list exprs))
  | LambdaSimple'(params, body) -> (empty_func (get_sexp_list body))
  | LambdaOpt'(params, opt_param, body) -> (empty_func (get_sexp_list body))
  | Applic'(operator, args) -> (let a = [get_sexp_list operator] in
                                  let b = List.map get_sexp_list args in
                                  empty_func [a; b])
  | ApplicTP'(operator, args) -> (let a = [get_sexp_list operator] in
                                    let b = List.map get_sexp_list args in
                                    empty_func [a; b]));;

  let flatten_sexp expr'_list = List.map get_sexp_list expr'_list;;

  let rec get_index_from_table const_table sexp = 
    (match const_table with
    | (Sexpr(sexpr), (offset, representation)) :: cdr when (sexpr_eq sexpr sexp) -> offset
    | car :: cdr -> get_index_from_table cdr sexp
    | [] -> (-1));;

  let rec find_sexp sexp_list sexp = 
    (match sexp_list with
    | String(str) :: rest when (sexpr_eq (String(str)) sexp) -> true
    | car :: cdr -> find_sexp cdr sexp
    | [] -> false);;  

  let sexp_to_string sexp = (match sexp with
    | Number(Fraction(x,y)) -> "("^string_of_int(x)^","^string_of_int(y)^")"
    | Number(Float(num)) -> string_of_float num
    | Char(ch) -> string_of_int(int_of_char(ch))
    | String(str) -> str
    | Symbol(str) -> str
    | _ -> "");;
    
  let rec sexp_const_temp sexp   = (match sexp with
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
    | Pair(x,y)-> (let car_pair = temp_list := List.append !temp_list [(Sexpr(x), true)] in
      let cdr_pair = sexp_const_temp y in
      let z = temp_list:=List.append !temp_list [(Sexpr(sexp), false)] in
      empty_func [car_pair, cdr_pair,z])
    | _ -> ());;  
  
  let rec make_temp_from_sexp_list sexp_list = 
    (match sexp_list with
    | [] -> []
    | car :: cdr -> 
      let first_sexp = sexp_const_temp car in
      take_second first_sexp (make_temp_from_sexp_list cdr));;

  let offset = ref 6;;
  
  let make_cosnt_sexp sexp = 
    (match sexp with
    | (Sexpr(sexpr),true) -> (let x = final_list := List.append !final_list [(Sexpr(sexpr),(!offset,"MAKE_LITERAL_RATIONAL"^sexp_to_string sexpr^""))] in
      let add_to_offset = offset := !offset + 17 in
      empty_func [x,add_to_offset])
    | (Sexpr(Number(Fraction(x,y))),false) -> (let x = final_list := List.append !final_list [(Sexpr(Number(Fraction(x,y))),(!offset,"MAKE_LITERAL_INT"^sexp_to_string (Number(Fraction(x,y)))^""))] in
    let add_to_offset = offset := !offset + 9 in
    empty_func [x,add_to_offset])
    | (Sexpr(Number(Float(num))),false) -> (let x = final_list := List.append !final_list [(Sexpr(Number(Float(num))),(!offset,"MAKE_LITERAL_INT"^sexp_to_string (Number(Float(num)))^""))] in
    let add_to_offset = offset := !offset + 9 in
    empty_func [x,add_to_offset])
    | (Sexpr(Char(ch)),false) -> (let x = final_list := List.append !final_list [(Sexpr(Char(ch)),(!offset,"MAKE_LITERAL_CHAR"^sexp_to_string (Char(ch))^""))] in
    let add_to_offset = offset := !offset + 2 in
    empty_func [x,add_to_offset])
    | (Sexpr(String(str)),false) -> (let x = final_list := List.append !final_list [(Sexpr(String(str)),(!offset,"MAKE_LITERAL_STRING(\""^str^"\")"))] in
    let add_to_offset = offset := !offset + 9 + (String.length str) in
    empty_func [x,add_to_offset])
    | (Sexpr(Symbol(str)),false) -> (let x = final_list := List.append !final_list [(Sexpr(Symbol(str)),(!offset,"MAKE_LITERAL_SYMBOL(const_tbl+"^string_of_int (get_index_from_table !final_list (String(str)))^")"))] in
    let add_to_offset = offset := !offset + 9 in
    empty_func [x,add_to_offset])
    | (Sexpr(Pair(x,y)),false) -> (let x = final_list := List.append !final_list [(Sexpr(Pair(x,y)),(!offset,"MAKE_LITERAL_PAIR(const_tbl+"^string_of_int (get_index_from_table !final_list x)^", const_tbl+"^string_of_int (get_index_from_table !final_list y)^")"))] in
    let add_to_offset = offset := !offset + 17 in
    empty_func [x,add_to_offset])
    | _ -> ());;

   let get_const_tables expr'_list = 
    let first_step = flatten_sexp expr'_list in
    let second_step = make_temp_from_sexp_list !sexp_list  in
    let third_step = List.map make_cosnt_sexp !temp_list in 
    let ans = List.append !final_list [] in
    let clean = final_list := [(Void, (0,"MAKE_VOID"));
    (Sexpr(Nil),(1,"MAKE_NIL"));
    (Sexpr(Bool false),(2,"MAKE_BOOL(0)"));
    (Sexpr(Bool true),(4,"MAKE_BOOL(1)"))] in
    let clean2 = sexp_list := [] in
    let clean3 = temp_list := [] in
    let clean4 = offset := 6 in
    let empty_all = empty_func [first_step, second_step, third_step, clean, clean2, clean3, clean4] in
    take_second empty_all ans;; 

     (* end of const tables *)

module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = get_const_tables asts;; 
  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;
