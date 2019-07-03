#use "big_step.ml"

let num_test = evaluate [] (Ncte(1));; (* deve retornar  Vnum 1 *)

let div_zero_test = evaluate [] (Binop(Div, Ncte(10), Ncte(0)));; (* deve retornar  RRaise *)

let mult_test = evaluate [] (Binop(Mult, Ncte(2), Ncte(5)));; (* deve retornar  10 *)

let div_test = evaluate [] (Binop(Div, Ncte(10), Ncte(2)));; (* deve retornar  Vnum 5 *)

let not_test = evaluate [] (Unop(Not, Bcte(true)));; (* deve retornar  Vbool false *)

let pair_test = evaluate [] (Pair(Ncte(1), Bcte(true)));; (* deve retornar  Vpair(Vnum 1, Vbool true) *)

let if_test = evaluate [] (If(Bcte(true), Ncte(10), Ncte(50)));; (* deve retornar  Vnum 10 *)

let if_test2 = evaluate [] (If(Bcte(true), Ncte(10), Bcte(false)));; (* deve retornar  RRaise *)

let cons_test = evaluate [] (Cons(Ncte(2), Ncte(5)));; (* deve retornar  Vcons(Vnum 2, Vnum 5) *)

let hd_test = evaluate [] (Hd(Cons(Ncte(2), Cons(Ncte(5), Nil))));; (* deve retornar  Vnum 2 *)

let tl_test = evaluate [] (Tl(Cons(Ncte(2), Cons(Ncte(5), Nil))));; (* deve retornar  Vcons(Vnum 5, Vnil) *)

let isempty_test = evaluate [] (IsEmpty(Cons(Bcte(true), Bcte(false))));; (* deve retornar  Vbool false *)

let try_test = evaluate [] (Try((Binop(Le, Ncte(5), Ncte(2))), (Binop(And, Bcte(true), Bcte(true)))));; (* deve retornar  Vbool false *)

let try_test_ex = evaluate [] (Try((Binop(Sum, Ncte(5), Bcte(true))), (Binop(Or, Bcte(true), Bcte(false)))));; (* deve retornar  Vbool true *)

let mult_test_r = evaluate [] (Binop(Mult, Ncte(2), Bcte(true)));; (* deve retornar  RRaise *)
