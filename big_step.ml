#use "sintaxe.ml"

exception InputInvalido of string

let rec evaluate (envmnt : env) (e : expr) = (
	match e with
		(* BS-Num *)
		Ncte(n) -> Vnum(n)
		(* BS-BinOp *)
		| Binop(op, e1, e2) -> (
		let e1' = evaluate envmnt e1 in (
		  if e1' = RRaise then RRaise else (
		    let e2' = evaluate envmnt e2 in (
		      if e2' = RRaise then RRaise else (
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
					| (_, _, _) -> raise (InputInvalido "Erro no type_infer")
		        )
		      )
		    )
		  )
		)
		(* BS-Bool *)
		| Bcte(b) -> Vbool(b)
		(* BS-UnOp *)
		| Unop(op, e1) -> (
			let e1' = evaluate envmnt e1 in (
				if e1' = RRaise then RRaise else (
					match(op, e1') with
						  (Not, Vbool b1) -> Vbool(not b1)
						| (_, _) -> raise (InputInvalido "Erro no type_infer")
				)
			)
		)
		(* Pair rules *)
		| Pair(e1, e2) -> (
			let e1' = evaluate envmnt e1 in
				if e1' = RRaise then RRaise else (
					let e2' = evaluate envmnt e2 in (
						if e2' = RRaise then RRaise else (
							Vpair(e1', e2')
					)
				)
			)
		)
    (* BS-Fst *)
    | Fst(e1) -> (
			let e1' = evaluate envmnt e1 in (
				match(e1') with
				  Vpair(v1, v2) -> v1
				| RRaise -> RRaise
				| _ -> raise (InputInvalido "Erro no type_infer")
			)
		)
    (* BS-Snd *)
    | Snd(e1) -> (
			let e1' = evaluate envmnt e1 in (
				match(e1') with
				  Vpair(v1, v2) -> v2
				| RRaise -> RRaise
				| _ -> raise (InputInvalido "Erro no type_infer")
			)
		)
		(* If rules *)
		| If(e1, e2, e3) -> (
			let e1' = evaluate envmnt e1 in (
				if e1' = RRaise then RRaise else (
					match(e1') with
						(Vbool true) -> evaluate envmnt e2 (* BS-IfTr *)
						| (Vbool false) -> evaluate envmnt e3 (* BS-IfFls *)
						| _ -> raise (InputInvalido "Erro no type_infer")
				)
			)
		)
		(* Application rules *)
		| App(e1, e2) -> let e1' = evaluate envmnt e1 in
					if e1' = RRaise then RRaise else (
							let e2' = evaluate envmnt e2 in (
								if e2' = RRaise then RRaise else (
								match(e1', e2') with
									  (Vclos(x, e', envmnt'), v) -> evaluate ((x, v)::envmnt') e'
									| (Vrclos(f, x, e', envmnt'), v) -> evaluate ((x, v)::(f, Vrclos(f, x, e', envmnt'))::envmnt') e'
									| (_, _) -> raise (InputInvalido "Erro no type_infer")
							)
						)
					)
		(* BS-Fn *)
		| Fn(x, e1) -> Vclos(x, e1, envmnt)
		(* BS-Let *)
		| Let(x, e1, e2) -> let e1' = evaluate envmnt e1 in (
								match(e1') with
									  RRaise -> RRaise
									| _ -> evaluate ((x, e1')::envmnt) e2
							)
		(* BS-LetRec *)
		| Letrec(f, x, e1, e2) -> let rclos = Vrclos(f, x, e1, envmnt) in evaluate ((f, rclos)::envmnt) e2
		(*List extension rules *)
		| Nil -> Vnil
		| Cons(e1, e2) -> (
			let e1' = evaluate envmnt e1 in
				if e1' = RRaise then RRaise else (
					let e2' = evaluate envmnt e2 in (
						if e2' = RRaise then RRaise else (
							Vcons(e1', e2')
						)
					)
				)
		)
		| Hd(e1) -> (
			let e1' = evaluate envmnt e1 in (
				if e1' = RRaise then RRaise else (
					match(e1') with
					  Vcons(v1, v2) -> v1
					| Vnil -> RRaise
					| _ -> raise (InputInvalido "Erro no type_infer")
				)
			)
		)
		| Tl(e1) -> (
			let e1' = evaluate envmnt e1 in (
				if e1' = RRaise then RRaise else (
					match(e1') with
					  Vcons(v1, v2) -> v2
					| Vnil -> RRaise
					| _ -> raise (InputInvalido "Erro no type_infer")
				)
			)
		)
		| IsEmpty(e1) -> (
			let e1' = evaluate envmnt e1 in (
				if e1' = RRaise then RRaise else (
					match(e1') with
					  Vcons(v1, v2) -> (Vbool false)
					| Vnil -> (Vbool true)
					| _ -> raise (InputInvalido "Erro no type_infer")
				)
			)
		)
		(* Exceptions extension rules *)
		| Raise -> RRaise
		| Try(e1, e2) -> (
			let e1' = evaluate envmnt e1 in (
				if e1' = RRaise then (evaluate envmnt e2) else e1'
			)
		)
		| _ -> raise (InputInvalido "Erro no type_infer")
)
