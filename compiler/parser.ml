#use "pc.ml";;

(* parse Atomics *)

let nt_withspaces  = PC.star(PC.char ' ');;

let digit = PC.range '0' '9';;

(*let boolean_val = PC.disj PC.char 't' PC.char 'f';;*)

let make_paired nt_left nt_right nt =
 let nt = PC.caten nt_left nt in
 let nt = PC.pack nt(function(_, e) -> e) in 
 let nt = PC.caten nt nt_right in 
 let nt = PC.pack nt(function(e, _) -> e) in
  nt;;

let make_spaced nt=
  make_paired nt_withspaces nt_withspaces nt;;

let tok_lparen = make_spaced(PC.char '(');;

let tok_rparen = make_spaced(PC.char ')');;

let tok_dot = make_spaced(PC.char '.');;


(*let parseBool = PC.caten PC.char '#' boolean_val;;*)

(*let parseBool_withspaces = make_spaced(parseBool);;*)


(* Sharon *)

(* Signs *)

let parse_positive_sign = make_spaced(PC.char '+');;

let parse_negative_sign = make_spaced(PC.char '-');;

let parse_division_sign = make_spaced(PC.char '/');;

let parse_dot_sign = make_spaced(PC.char '.');;

(* Combinations *)

let parse_sign = PC.disj parse_positive_sign parse_negative_sign;;

let parse_natural = PC.plus digit;;

let parse_signed_natural = PC.caten parse_sign parse_natural;;

(* Numbers *)

let parse_intiger = PC.disj (PC.pack parse_natural (fun (l) -> ('+',l))) parse_singed_natural;;

let parse_fraction = PC.caten (PC.caten parse_intiger parse_division_sign) parse_natural;;

let parse_float = PC.caten (PC.caten parse_intiger parse_dot_sign) parse_natural;;
