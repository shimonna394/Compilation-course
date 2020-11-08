#use "pc.ml";;
#use "reader.ml";;
open PC;;
open Reader;;
exception X_this_should_not_happen;;
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
  
  (* let nt_sexp_comments exp =
    let nt_start = word_ci "#;" in
    let nt = pack (caten nt_start all_exps) (fun (hash, exp) -> exp) in
    let nt = pack (caten nt all_exps) (fun (exp1, exp2) -> exp2) in
    (make_spaced_and_commented nt) exp;; *)
  

(* parse Atomics *)

let digit = PC.range '0' '9';;

let lower_case = PC.range 'a' 'z';;

let upper_case = PC.range 'A' 'Z';;

let dot = PC.char '.';;

let parse_pref = PC.char '#';;

let parse_slash = PC.char '/';;

let nt_lparen = make_spaces_and_comments(PC.char '(');;

let nt_rparen = make_spaces_and_comments(PC.char ')');;

(* parse booleans *)

let parse_booleans exp =
  let nt = disj (word_ci "#f") (word_ci "#t") in
  let nt_val  = pack nt (fun (prefix) -> match((list_to_string prefix)) with
      | "#f" -> Bool(false)
      | "#F" -> Bool(false)
      | "#t" -> Bool(true)
      | "#T" -> Bool(true)
      | _ -> raise X_no_match) in
  let nt_val = make_spaced nt_val in
  (make_spaces_and_comments nt_val) exp;;

(* parse nil *)

let parse_nil =
  let nt_nil = pack (caten nt_lparen nt_rparen) (fun (lparen, rparen) -> Nil) in
  let nt_nil = make_spaces_and_comments nt_nil in
  nt_nil;;

(* parse symbol *)  

(*let parse_symbol = 
  let parse_SymbolCharNoDot = PC.disj_list[digit;lower_case;upper_case;(PC.char '!');
  (PC.char '$');(PC.char '^');(PC.char '*');(PC.char '-');(PC.char '_');(PC.char '=');
  (PC.char '+');(PC.char '<');(PC.char '>');(PC.char '?');(PC.char '/');(PC.char ':')] in
  let parse_SymbolCharNoDot = PC.pack parse_SymbolCharNoDot (lowercase_ascii) in
  let parse_symbolChar = PC.disj parse_SymbolCharNoDot dot in
  let plus_symbol_char = PC.plus(parse_symbolChar) in
  let parse_symbol = PC.disj parse_SymbolCharNoDot plus_symbol_char in
  let parse_symbol = PC.pack (parse_symbol) (fun (prefix) ->  (Symbol(list_to_string prefix))) in
  make_spaces_and_comments parse_symbol;; *)

(* parse char *)



(* Sharon *)

(* Signs *)

let parse_positive_sign = make_spaced(PC.char '+');;

let parse_negative_sign = make_spaced(PC.char '-');;

let parse_division_sign = make_spaced(PC.char '/');;

let parse_dot_sign = make_spaced(PC.char '.');;

(* Combinations *)

let parse_sign = PC.disj parse_positive_sign parse_negative_sign;;

(*PC.pack (PC.caten (PC.star (PC.char '0')) (PC.plus digit)) (fun (_,l) -> l)*)
let parse_natural = PC.plus digit;;

let parse_signed_natural = PC.caten parse_sign parse_natural;;

(* Numbers *)

let parse_integer = PC.disj (PC.pack parse_natural (fun (l) -> ('+',l))) parse_signed_natural;;

let parse_fraction = PC.caten (PC.caten parse_integer parse_division_sign) parse_natural;;

let parse_float = PC.caten (PC.caten parse_integer parse_dot_sign) parse_natural;;

let parse_number = 
  let float_integer = PC.pack parse_integer (fun (l) -> ((l, '.'), ['0'])) in 
  PC.disj (PC.disj parse_fraction parse_float) float_integer;;

  (* String and Chars *)
    let parse_string_meta_char = 
      let backslash = make_spaced(PC.char '\\')
      and quote = make_spaced(PC.char '\"')
      and tab = make_spaced(PC.char '\t') 
      and new_line = make_spaced(PC.char '\n') in
      PC.disj_list [backslash; quote; tab; new_line];;