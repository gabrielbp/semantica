#use "sintaxe.ml"
#use "resultados.ml"

let rec evaluate (envmnt : env) (e : expr) = (
	match e with
		(* BS-Num *)
		Ncte(n) -> Vnum(n)
		(* BS-Bool *)
		| Bcte(b) -> Vbool(b)
		(* BS-BinOp *)
		| Binop(op, e1, e2) -> (
			let e1' = evaluate envmnt e1 in
				let e2' = evaluate envmnt e2 in (
					match(op, e1', e2') with
						  (Sum, Vnum n1, Vnum n2) -> Vnum(n1 + n2)
						| (Sub, Vnum n1, Vnum n2) -> Vnum(n1 - n2)
						| (Mult, Vnum n1, Vnum n2) -> Vnum(n1 * n2)
						| (Div, Vnum n1, Vnum n2) -> if n2 != 0 then Vnum(n1 / n2) else RRaise
						| (And, Vbool b1, Vbool b2) -> Vbool(b1 && b2)
						| (Or, Vbool b1, Vbool b2) -> Vbool(b1 || b2)
						| (Eq, Vnum n1, Vnum n2) -> Vbool(n1 == n2)
						| (Df, Vnum n1, Vnum n2) -> Vbool(n1 != n2)
						| (Lt, Vnum n1, Vnum n2) -> Vbool(n1 < n2)
						| (Le, Vnum n1, Vnum n2) -> Vbool(n1 <= n2)
						| (Gt, Vnum n1, Vnum n2) -> Vbool(n1 > n2)
						| (Ge, Vnum n1, Vnum n2) -> Vbool(n1 >= n2)
						| (_, RRaise, _) -> RRaise
						| (_, _, RRaise) -> RRaise
						| (_, _, _) -> RRaise
				)
		)
		(* BS-UnOp *)
		| Unop(op, e1) -> (
			let e1' = evaluate envmnt e1 in (
				match(op, e1') with
					  (Not, Vbool b1) -> Vbool(not b1)
					| (_, RRaise) -> RRaise
					| (_, _) -> RRaise
			)
		)
		(* Pair rules *)
		| Pair(e1, e2) -> (
			let e1' = evaluate envmnt e1 in
				let e2' = evaluate envmnt e2 in (
					match(e1',e2') with
						  (RRaise, _) -> RRaise
						| (_, RRaise) -> RRaise
						| (_, _) -> Vpair(e1', e2')
				)
		)
		(* If rules *)
		| If(e1, e2, e3) -> ( 
			let e1' = evaluate envmnt e1 in (
				match(e1') with
					(Vbool true) -> evaluate envmnt e2 (* BS-IfTr *)
					| (Vbool false) -> evaluate envmnt e3 (* BS-IfFls *)
					| RRaise -> RRaise (* BS-IfRaise *)
					| _ -> RRaise
			)
		)
		(* BS-Id *)
		| Var(x) -> let rec search_env (envmnt : env) (x : variable) = (
						match(envmnt) with
							 [] -> RRaise
							| (var, res)::rest_env when var = x -> res
							| _ -> let rest_env = List.tl envmnt in search_env rest_env x
					) in search_env envmnt x
		(* Application rules *)
		| App(e1, e2) -> let e1' = evaluate envmnt e1 in
							let e2' = evaluate envmnt e2 in (
								match(e1', e2') with
									  (Vclos(x, e', envmnt'), v) -> evaluate ((x, v)::envmnt') e'
									| (Vrclos(f, x, e', envmnt'), v) -> evaluate ((x, v)::(f, Vrclos(f, x, e', envmnt'))::envmnt') e'
									| (RRaise, _) -> RRaise
									| (_, RRaise) -> RRaise
									| (_, _) -> RRaise
							)
		(* BS-Fn *)
		| Lam(x, e1) -> Vclos(x, e1, envmnt)
		(* BS-Let *)
		| Let(x, e1, e2) -> let e1' = evaluate envmnt e1 in (
								match(e1') with
									  RRaise -> RRaise
									| _ -> evaluate ((x, e1')::envmnt) e2
							)
		(* BS-LetRec *)
		| Lrec(f, x, e1, e2) -> let rclos = Vrclos(f, x, e1, envmnt) in evaluate ((f, rclos)::envmnt) e2
		(*List extension rules *)
		| Nil -> Vnil
		| Cons(e1, e2) -> (
			let e1' = evaluate envmnt e1 in
				let e2' = evaluate envmnt e2 in (
					match(e1', e2') with
						  (RRaise, _) -> RRaise
						| (_, RRaise) -> RRaise
						| (_, _) -> Vcons(e1', e2')
				)
		)
		| Hd(e1) -> (
			let e1' = evaluate envmnt e1 in (
				match(e1') with
				  Vcons(v1, v2) -> v1
				| Vnil -> RRaise
				| RRaise -> RRaise
				| _ -> RRaise
			)
		)
		| Tl(e1) -> (
			let e1' = evaluate envmnt e1 in (
				match(e1') with
				  Vcons(v1, v2) -> v2
				| Vnil -> RRaise
				| RRaise -> RRaise
				| _ -> RRaise
			)
		)
		| IsEmpty(e1) -> (
			let e1' = evaluate envmnt e1 in (
				match(e1') with
				  Vcons(v1, v2) -> (Vbool false)
				| Vnil -> (Vbool true)
				| RRaise -> RRaise
				| _ -> RRaise
			)
		)
		(* Exceptions extension rules *)
		| Raise -> RRaise
		| Try(e1, e2) -> (
			let e1' = evaluate envmnt e1 in (
				match(e1') with
					  RRaise -> evaluate envmnt e2
					| _ -> e1'
			)
		)
		| _ -> RRaise
)