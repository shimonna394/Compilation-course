
#use "pc.ml";;
open PC;;
exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2);;

  (* Shimi *)

(* General functions *)

let nt_withspaces= star(const (fun ch -> ch <= ' '));; 

let make_paired nt_left nt_right nt =
  let nt = PC.caten nt_left nt in
  let nt = PC.pack nt(function(_, e) -> e) in 
  let nt = PC.caten nt nt_right in 
  let nt = PC.pack nt(function(e, _) -> e) in
   nt;;
 
 let make_spaced nt=
   make_paired nt_withspaces nt_withspaces nt;;

  let nt_comments = 
    let nt_semicolon = PC.char ';' in
    let nt_rest = star (const (fun ch -> (ch != (char_of_int 4)) && (ch != (char_of_int 10)))) in
    let nt_comments = pack (caten nt_semicolon nt_rest) (fun (e1, e2) -> e1 :: e2) in
    let ignore_spaces = make_spaced nt_comments in
    PC.star(ignore_spaces);;
    
  let make_comments nt = 
    make_paired nt_comments nt_comments nt;;
    
  let make_spaces_and_comments nt = 
    make_comments (make_spaced(nt));;
   

(* parse Atomics *)

let digit = PC.range '0' '9';;

let lower_case = PC.range 'a' 'z';;

let upper_case = PC.range 'A' 'Z';;

let dot = PC.word_ci ".";;

let nt_lparen = make_spaces_and_comments (PC.char '(');;

let nt_rparen = make_spaces_and_comments (PC.char ')');;


(* Signs *)

let parse_positive_sign = make_spaced(PC.char '+');;

let parse_negative_sign = make_spaced(PC.char '-');;

let parse_division_sign = make_spaced(PC.char '/');;

let parse_dot_sign = make_spaced(PC.char '.');;

let parse_n = PC.disj (PC.char 'e') (PC.char 'E');;

(* Combinations *)

let parse_sign = PC.disj parse_positive_sign parse_negative_sign;;

let parse_natural = PC.plus digit;;

let parse_signed_natural = PC.caten parse_sign parse_natural;;

let parse_integer = PC.disj (PC.pack parse_natural (fun (l) -> ('+',l))) parse_signed_natural;;

let parse_fraction = PC.caten (PC.caten parse_integer parse_division_sign) parse_natural;;

let parse_float = PC.caten (PC.caten parse_integer parse_dot_sign) parse_natural;;

let parse_number = 
  let float_integer = PC.pack parse_integer (fun (l) -> ((l, '.'), ['0'])) in 
  PC.disj (PC.disj parse_fraction parse_float) float_integer;;

let parse_sci_not exp = 
  let parse_integer_in_float_stracture = PC.pack parse_integer (fun (l) -> ((l, '.'), ['0'])) in
  try (PC.caten (PC.caten parse_integer_in_float_stracture parse_n) parse_integer) exp
  with PC.X_no_match ->
  try (PC.caten (PC.caten parse_float parse_n) parse_integer) exp
  with PC.X_no_match -> raise X_no_match;;


let make_parens nt = 
  make_paired nt_lparen nt_rparen nt;; 

  (*                                          EVAL NUMBERS                                              *)  
(* -------------------------------------------------------------------------------------------------- *)

let parsed_integer_to_int_type parsed_exp = 
  let sign = (fun (l,_) -> l) parsed_exp 
  and num = (fun (_,r) -> r) parsed_exp
  in int_of_string((String.make 1 sign) ^ list_to_string(num));

and parsed_integer_to_float_type parsed_exp =
          let num = (fun (sign, num) -> float_of_string((String.make 1 sign) ^ list_to_string(num) ^ ".0")) parsed_exp
          in num;

and parsed_float_to_float_type parsed_exp = 
    let float_num = (fun ((((sign, num),div),frac),r) -> float_of_string((String.make 1 sign) ^ list_to_string(num) ^ "." ^ list_to_string(frac))) parsed_exp   
        in float_num;;


let rec gcd x y = 
  if y = 0 then x
  else (gcd y (x mod y));;

  let eval_fraction exp =
    let parsed_exp = parse_fraction exp in
    let nt = (fun (l, r) -> l)parsed_exp
    and rest = (fun (l,r) ->r)parsed_exp in
    let numerator = (fun (((sign, num),div),frac) -> int_of_string((String.make 1 sign) ^ list_to_string(num))) nt   
    and denominator = (fun (((sign, num),div),frac) -> int_of_string(list_to_string(frac))) nt in 
    let gcdVal = (gcd numerator denominator) in        
    (Number(Fraction(numerator / gcdVal, denominator / gcdVal)), rest);;
  
  let eval_float exp = 
    let parsed_exp = parse_float exp in
    let rest = (fun (l,r) -> r)parsed_exp in
    (Number(Float(parsed_float_to_float_type parsed_exp)),rest);;
  
  let eval_int exp =
    let parsed_exp = parse_integer exp in
    let num = (fun ((sign, num),r) -> int_of_string((String.make 1 sign) ^ (list_to_string num)))parsed_exp 
    and rest = (fun ((sign, num),r) -> r)parsed_exp in
    (Number(Fraction(num, 1)),rest);;
  
  
  let eval_sci_no exp = 
    let parsed_exp = parse_sci_not exp in
    let rest = (fun (l,r) -> r)parsed_exp in
    let eval_first_float = parsed_float_to_float_type((fun ((l,r),k) -> l) parsed_exp) 
    and eval_second_float = parsed_integer_to_float_type((fun ((l,r),k) -> r) parsed_exp) in
    let exp_result = 10.0 ** eval_second_float in
    (Number(Float(eval_first_float *. exp_result)), rest);;
    
  
   (* try eval_float exp
    with PC.X_no_match ->
    try eval_sci_no exp
    with PC.X_no_match ->
    try eval_int exp
    with PC.X_no_match ->
    try eval_fraction exp
    with PC.X_no_match ->
    try eval_float exp
    with PC.X_no_match ->
    try eval_sci_no exp
    with PC.X_no_match -> raise X_no_match;; *)
  
  (* -------------------------------------------------------------------------------------------------- *)

  (* String and Chars *)

let parse_quote_sign = PC.disj (make_spaced(PC.char '\"')) (make_spaced(PC.char '"'));;

let parse_string_meta_char = 
  let backslash = make_spaced(PC.char '\\')
  and quote = make_spaced(PC.char '\"')
  and tab = make_spaced(PC.char '\t') 
  and newFeed = make_spaced(PC.char (char_of_int 12)) (*\f*)
  and new_line = make_spaced(PC.char '\n') in
  (PC.disj_list [backslash; quote; tab; new_line; newFeed]);;
    
let parse_string_literal_char =
  PC.const (fun (c) -> c != '\\' && c != '"');;

let parse_string_char = PC.star (PC.disj parse_string_literal_char parse_string_meta_char);;
    
let parse_string = 
  PC.caten (PC.caten parse_quote_sign parse_string_char) parse_quote_sign;;

let rec all_sexp sexp = (PC.disj_list [parse_booleans; parse_char;eval_number;parse_symbol;parse_nil;parse_list;
parse_dotted_list;parse_quoted;parse_qquoted;parse_unquoted;parse_unquoted_spliced;parse_sexp_comments]) sexp;


(* parse booleans *)

and parse_booleans sexp =
  let nt = disj (word_ci "#f") (word_ci "#t") in
  let nt_bool  = pack nt (fun (prefix) -> match((String.lowercase_ascii (list_to_string prefix))) with
      | "#f" -> Bool(false)
      | "#t" -> Bool(true)
      | _ -> raise X_no_match) in
  let nt_bool = make_spaced nt_bool in
  (make_spaces_and_sexp_comments nt_bool) sexp;

(* parse nil *)

and parse_nil sexp =
  let nt_nil = pack (make_paired nt_lparen nt_rparen (star (caten (word_ci "#;") all_sexp))) (fun (sexp) -> Nil) in
  let nt_nil = make_spaces_and_sexp_comments nt_nil in
  nt_nil sexp;

(* parse number *)

and eval_number exp = 
  (make_spaces_and_sexp_comments (PC.disj_list [eval_sci_no;eval_float;eval_fraction;eval_int])) exp;

(* parse symbol *)  

and parse_symbol sexp = 
  let parse_SymbolCharNoDot = PC.disj_list[(PC.plus digit);(PC.plus lower_case);(PC.plus upper_case);
  (PC.word_ci "!");(PC.word_ci "$");(PC.word_ci "^");(PC.word_ci "*");
  (PC.word_ci "-");(PC.word_ci "_");(PC.word_ci "=");(PC.word_ci "+");
  (PC.word_ci "<");(PC.word_ci ">");(PC.word_ci "?");(PC.word_ci "/");(PC.word_ci ":")] in
  let parse_SymbolCharNoDot = PC.pack parse_SymbolCharNoDot (List.map lowercase_ascii) in
  let parse_symbolChar = PC.disj dot parse_SymbolCharNoDot in
  let plus_symbol_char = PC.pack (PC.plus(parse_symbolChar)) (fun (list_of_lists) ->
  List.flatten list_of_lists) in
  let plus_symbol_char = PC.pack (PC.caten parse_symbolChar plus_symbol_char) (fun (e,s) ->
  List.append e s) in
  let parse_symbol = PC.pack (disj plus_symbol_char parse_SymbolCharNoDot) (fun (prefix) ->  (Symbol(list_to_string prefix))) in
  (make_spaces_and_sexp_comments parse_symbol) sexp;

(* parse char *)

and parse_char sexp = 
  let parse_char_prefix = PC.word_ci "#\\" in
  let parse__visible_simple_char = const (fun ch -> ch > ' ') in
  let parse_named_char = PC.disj_list [(PC.word_ci "newline");(PC.word_ci "nul");
  (PC.word_ci "page");(PC.word_ci "return");(PC.word_ci "space");(PC.word_ci "tab")] in
  let parse__visible_simple_char = pack (caten parse__visible_simple_char nt_epsilon) (fun (e,s) -> e::s)in
  let parse_char = caten (parse_char_prefix) (disj parse_named_char parse__visible_simple_char) in
  let parse_char = pack parse_char (fun (prefix,rest) -> match ((list_to_string prefix), (String.lowercase_ascii (list_to_string rest))) with
      | ("#\\", "tab") -> Char '\t'
      | ("#\\", "newline") -> Char '\n'
      | ("#\\", "space") -> Char ' '
      | ("#\\", "return") -> Char '\r'
      | ("#\\", "page") -> Char '\012'
      | ("#\\", "nul") -> Char '\000'
      | ("#\\", c) -> Char (list_to_string rest).[0]
      | (_, _) -> raise X_no_match) in
  (make_spaces_and_sexp_comments parse_char) sexp;

(* parse pair *)

and parse_list sexp= 
  let sexp_list = make_parens (PC.star all_sexp) in
  let sexp_list = PC.pack (sexp_list) (fun sexps ->
  List.fold_left (fun x y -> Pair(y, x)) Nil (List.rev sexps)) in
  (make_spaces_and_sexp_comments sexp_list) sexp;
  
and parse_dotted_list sexp = 
  let before_dot = make_spaces_and_comments (PC.plus all_sexp) in
  let with_no_dot = PC.pack (caten before_dot dot) (fun (before,dot) -> before) in
  let dotted_list = PC.pack (caten with_no_dot all_sexp) (fun (before_dot,after_dot) ->
  List.fold_right (fun x y -> match y with Nil -> x | _ -> Pair(x,y)) (List.append before_dot (after_dot::[])) Nil) in
  (make_spaces_and_sexp_comments (make_parens dotted_list)) sexp;


(* parse quote *)

and parse_quoted sexp = 
  let char_quote = PC.char (char_of_int 39) in
  let quote_sexp = PC.pack (PC.caten char_quote all_sexp) (fun (quote,sexp)->
  Pair(Symbol("quote"),Pair(sexp,Nil))) in
  (make_spaces_and_sexp_comments quote_sexp) sexp;

and parse_qquoted sexp = 
  let char_qquoted = PC.char (char_of_int 96) in
  let qquoted_sexp = PC.pack (PC.caten char_qquoted all_sexp) (fun (quote,sexp)->
  Pair(Symbol("quasiquote"),Pair(sexp,Nil))) in
  (make_spaces_and_sexp_comments qquoted_sexp) sexp;

and parse_unquoted sexp = 
  let char_unquoted = PC.char (char_of_int 44) in
  let unquoted_sexp = PC.pack (PC.caten char_unquoted all_sexp) (fun (quote,sexp)->
  Pair(Symbol("unquote"),Pair(sexp,Nil))) in
  (make_spaces_and_sexp_comments unquoted_sexp) sexp;

and parse_unquoted_spliced sexp = 
  let char_unquoted_spliced = PC.word_ci ",@" in
  let unquoted_spliced_sexp = PC.pack (PC.caten char_unquoted_spliced all_sexp) (fun (quote,sexp)->
  Pair(Symbol("unquote-splicing"),Pair(sexp,Nil))) in
  (make_spaces_and_sexp_comments unquoted_spliced_sexp) sexp;

and parse_sexp_comments sexp =
  let start = word_ci "#;" in
  let sexp_comments = pack (caten start all_sexp) (fun (start, sexp) -> sexp) in
  let sexp_comments = pack (caten sexp_comments all_sexp) (fun (sexp1, sexp2) -> sexp2) in
  (make_spaces_and_sexp_comments sexp_comments) sexp;

and make_spaces_and_sexp_comments nt =
  (make_paired (star (caten (word "#;") all_sexp)) (star (caten (word "#;") all_sexp)) (make_spaces_and_comments nt));;

(* Sharon *)

(* -----------Numbers------------- *)



  
module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


let read_sexprs string = 
  let read string = (match ((plus all_sexp) (string_to_list string)) with
    | (sexps, chs) -> sexps) in
  try (read string)
  with X_no_match -> [];;

end;; (* struct Reader *)

