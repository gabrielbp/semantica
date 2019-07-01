#use "big_step.ml"

let num_test = evaluate [] (Ncte(1));; (* must return Vnum 1 *)

let div_test = evaluate [] (Binop(Div, Ncte(10), Ncte(2)));; (* must return Vnum 5 *)

let mult_test_r = evaluate [] (Binop(Mult, Ncte(2), Bcte(true)));; (* must return RRaise *)

let not_test = evaluate [] (Unop(Not, Bcte(true)));; (* must return Vbool false *)

let pair_test = evaluate [] (Pair(Ncte(1), Bcte(true)));; (* must return Vpair(Vnum 1, Vbool true) *)

let if_test = evaluate [] (If(Bcte(true), Ncte(10), Ncte(50)));; (* must return Vnum 10 *)

let id_test = evaluate [("x", Vnum(5))] (Var("x"));; (* must return Vnum 5 *)

let id_test_r = evaluate [("x", Vnum(5))] (Var("y"));; (* must return RRaise *)

let fn_test = evaluate [] (Fun("x", Var("x")));; (* identity function: must return Vclos("x", Var "x", []) *)

let app_test = evaluate [] (App(App(Fun( "x",
					Try(If( Binop(Gt, Var("x"), Ncte(0)),
						Fun("y", Binop(Sum, Var("y"), Ncte(1))),
						Fun("z", Var("z"))
					    ),
					    Fun("w", Var("w"))
					)
				   ),
				   Ncte(4)),
				Ncte(5)
			    ));; (* p2 2017 question 2: must return Vnum 6 *)

let let_test = evaluate [] (Let("x", Binop(Sum, Ncte(5), Ncte(3)), Binop(Mult, Var("x"), Var("x"))));; (* must return Vnum 64 *)

let letrec_test = evaluate [] (Lrec("fat",
				    "x",
				    If(Binop(Eq, Var("x"), Ncte(0)),
						Ncte(1),
						Binop(Mult,
						      Var("x"),
						      App(Var("fat"),
							  Binop(Sub, Var("x"), Ncte(1))
						      )
						)
				    ),
				    App(Var("fat"), Ncte(6))
			       )
			      );; (* fat 6: must return Vnum 720 *)

let cons_test = evaluate [] (Cons(Ncte(2), Ncte(5)));; (* must return Vcons(Vnum 2, Vnum 5) *)

let hd_test = evaluate [] (Hd(Cons(Ncte(2), Cons(Ncte(5), Nil))));; (* must return Vnum 2 *)

let tl_test = evaluate [] (Tl(Cons(Ncte(2), Cons(Ncte(5), Nil))));; (* must return Vcons(Vnum 5, Vnil) *)

let isempty_test = evaluate [] (IsEmpty(Cons(Bcte(true), Bcte(false))));; (* must return Vbool false *)

let try_test = evaluate [] (Try((Binop(Le, Ncte(5), Ncte(2))), (Binop(And, Bcte(true), Bcte(true)))));; (* must return Vbool false *)

let try_test_ex = evaluate [] (Try((Binop(Sum, Ncte(5), Bcte(true))), (Binop(Or, Bcte(true), Bcte(false)))));; (* must return Vbool true *)
