#use "pc.ml";;

(* parse Atomics *)

let nt_withspaces  = PC.star(PC.char ' ');;

let digit = PC.range '0' '9';;

let boolean_val = PC.disj PC.char 't' PC.char 'f';;

let make_paired nt_left nt_right nt=
 let nt = PC.caten nt_left nt in
 let nt = PC.pack nt(function(_, e) -> e) in 
 let nt = PC.caten nt nt_right in 
 let nt = PC.pack nt(function(e, _) -> e) in
  nt;;

let make_spaced nt=
  make_paired nt_withspaces nt_withspaces nt;;

let tok_lparen= make_spaced( PC.char '(');;

let tok_rparen= make_spaced( PC.char ')');;

let tok_dot= make_spaced( PC.char '.');;


let parseBool = PC.caten PC.char '#' boolean_val;;

let parseBool_withspaces = make_spaced(parseBool);;


