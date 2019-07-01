#use "type_infer.ml"

let num_test = get_constraints [] (Ncte(1));; (* deve retornar  (TyInt, []) *)

let div_test = get_constraints [] (Binop(Div, Ncte(10), Ncte(2)));; (* deve retornar  Vnum 5 *)

let mult_test_r = get_constraints [] (Binop(Mult, Ncte(2), Bcte(true)));; (* deve retornar  RRaise *)

let not_test = get_constraints [] (Unop(Not, Bcte(true)));; (* deve retornar  Vbool false *)

let pair_test = get_constraints [] (Pair(Ncte(1), Bcte(true)));; (* deve retornar  Vpair(Vnum 1, Vbool true) *)

let if_test = get_constraints [] (If(Bcte(true), Ncte(10), Ncte(50)));; (* deve retornar  Vnum 10 *)

let cons_test = get_constraints [] (Cons(Ncte(2), Ncte(5)));; (* deve retornar  Vcons(Vnum 2, Vnum 5) *)

let hd_test = get_constraints [] (Hd(Cons(Ncte(2), Cons(Ncte(5), Nil))));; (* deve retornar  Vnum 2 *)

let tl_test = get_constraints [] (Tl(Cons(Ncte(2), Cons(Ncte(5), Nil))));; (* deve retornar  Vcons(Vnum 5, Vnil) *)

let isempty_test = get_constraints [] (IsEmpty(Cons(Bcte(true), Bcte(false))));; (* deve retornar  Vbool false *)

let try_test = get_constraints [] (Try((Binop(Le, Ncte(5), Ncte(2))), (Binop(And, Bcte(true), Bcte(true)))));; (* deve retornar  Vbool false *)

let try_test_ex = get_constraints [] (Try((Binop(Sum, Ncte(5), Bcte(true))), (Binop(Or, Bcte(true), Bcte(false)))));; (* deve retornar  Vbool true *)
