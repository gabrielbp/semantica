#use "sintaxe.ml"

exception TypeNotFound of string
exception VariavelNaoDeclarada of string
exception FalhaNoUnify of string

let count = ref (-1);;
let newVar (c : int ref) = begin incr c; "X" ^ string_of_int (!c) end

let rec procuraNoAmbiente (envmnt : typeEnv) (x: string) =
		try let var = List.hd envmnt in
			match (var) with
			(variable, tipo) when variable = x -> tipo
			| _ -> procuraNoAmbiente (List.tl envmnt) x
		with _ -> raise (VariavelNaoDeclarada "Variavel nao foi declarada")


let rec get_constraints (envmnt : typeEnv) (e : expr) = (
	match e with
		  Ncte(n) -> (TyInt, [])
		| Bcte(b) -> (TyBool, [])
		| Binop(op, e1, e2) -> (
			match(op) with
			  Sum ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Sub ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Mult ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Div ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| And ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyBool); (typeE2, TyBool)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Or ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyBool); (typeE2, TyBool)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Eq ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Df ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Lt ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Le ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Gt ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Ge ->
					let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
		)
		| Unop(op, e1) -> (
			 match(op) with
			  Not -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let newConstraint = [(typeE1, TyBool)] in
							(TyBool, List.concat[newConstraint; constraintE1])
		)
		| Pair(e1, e2) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let (typeE2, constraintE2) = get_constraints envmnt e2 in
						(TyPair(typeE1, typeE2), List.concat[constraintE1; constraintE2])
		)
		| Fst(e1) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let newType1 = newVar count in
						let newType2 = newVar count in
							let newConstraint = [(typeE1, TyPair(TyId(newType1),TyId(newType2)))] in
								(TyId(newType1), List.concat[newConstraint; constraintE1])
		)
		| Snd(e1) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let newType1 = newVar count in
						let newType2 = newVar count in
							let newConstraint = [(typeE1, TyPair(TyId(newType1),TyId(newType2)))] in
								(TyId(newType2), List.concat[newConstraint; constraintE1])
		)
		| If(e1, e2, e3) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let (typeE2, constraintE2) = get_constraints envmnt e2 in
						let (typeE3, constraintE3) = get_constraints envmnt e3 in
							let newConstraint = [(typeE1, TyBool); (typeE2, typeE3)] in
								(typeE2, List.concat[newConstraint; constraintE1; constraintE2; constraintE3])
		)
		| App(e1, e2) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let (typeE2, constraintE2) = get_constraints envmnt e2 in
						let newType = newVar count in
							let newConstraint = [(typeE1, TyFn(typeE2, TyId(newType)))] in
								(TyId(newType), List.concat[newConstraint; constraintE1; constraintE2])
		)
		| Fn(x, e1) -> (
				let newType = newVar count in
					let newConstraint = [(x,TyId(newType))] in
						let (typeE1, constraintE1) = get_constraints (List.concat[newConstraint; envmnt]) e1 in
							(TyFn(TyId(newType),typeE1), List.concat[constraintE1])
		)
		| Let(x, e1, e2) -> (
				let (typeE1,constraintE1) = get_constraints envmnt e1 in
		      let newType1 = newVar count in
						let newConstraint1 = [(x,TyId(newType1))] in
							let (typeE2,constraintE2) = get_constraints (List.concat [newConstraint1;envmnt]) e2 in
								let newConstraint2 = [(TyId(newType1),typeE1)] in
						      ((TyId(newType1)), List.concat [constraintE1; constraintE2; newConstraint2])
		)
		| Letrec(x1, x2, e1, e2) -> (
				let newType1 = newVar count in
		      let newConstraint1 = [(x1,TyId(newType1))] in
			      let newType2 = newVar count in
							let newConstraint2 = [(x2,TyId(newType2))] in
								let (typeE1,constraintE1) = get_constraints (List.concat [newConstraint1;newConstraint2;envmnt]) e1 in
									let (typeE2,constraintE2) = get_constraints (List.concat [newConstraint1;envmnt]) e2 in
										let newConstraint3 = [(TyId(newType1),TyFn(TyId(newType2), typeE1))] in
											(typeE2, List.concat [constraintE1; constraintE2; newConstraint3])
		)
		| Nil -> (
				let newType = newVar count in (TyList(TyId(newType)), [])
		)
		| Cons(e1, e2) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let (typeE2, constraintE2) = get_constraints envmnt e2 in
						let newConstraint = [(TyList(typeE1), typeE2)] in
							(typeE2, List.concat[newConstraint; constraintE1; constraintE2])
		)
		| Hd(e1) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let newType = newVar count in
						let newConstraint = [(typeE1, TyList(TyId(newType)))] in
							(TyId(newType), List.concat[newConstraint; constraintE1])
		)
		| Tl(e1) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let newType = newVar count in
						let newConstraint = [(typeE1, TyList(TyId(newType)))] in
							(TyId(newType), List.concat[newConstraint; constraintE1])
		)
		| IsEmpty(e1) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let newType = newVar count in
						let newConstraint = [(typeE1, TyList(TyId(newType)))] in
							(TyBool, List.concat[newConstraint; constraintE1])
		)
		| Raise -> (
				let newType = newVar count in (TyId(newType), [])
		)
		| Try(e1, e2) -> (
				let (typeE1, constraintE1) = get_constraints envmnt e1 in
					let (typeE2, constraintE2) = get_constraints envmnt e2 in
						let newConstraint = [(typeE1, typeE2)] in
							(typeE1, List.concat[newConstraint; constraintE1; constraintE2])
		)
		| Id(x) -> (
				((procuraNoAmbiente envmnt x), [])
		)
)

let rec occurCheck (t: tipo) (x: variable) =
	match (t) with
		 TyInt  -> false
		|TyBool -> false
		|TyList(typeE) -> occurCheck typeE x
		|TyFn(typeE1, typeE2) ->
			if (occurCheck typeE1 x) then true else (occurCheck typeE2 x)
		|TyPair(typeE1, typeE2) ->
			if (occurCheck typeE1 x) then true else (occurCheck typeE2 x)
		|TyId(variavel) -> if variavel = x then true else false
		
(*
let rec unify constraints =
 	match(constraints) with
	  [] -> []
	| (TyInt, TyInt)::rest -> unify rest
	| (TyBool, TyBool)::rest -> unify rest
	| (TyList(typeE1), TyList(typeE2))::rest -> unify ((typeE1, typeE2)::rest)
	| (TyFn(typeE1, typeE2), TyFn(typeE3, typeE4))::rest -> unify ((typeE1, typeE3)::(typeE2, typeE4)::rest)
	| (TyPair(typeE1, typeE2), TyPair(typeE3, typeE4))::rest -> unify ((typeE1, typeE3)::(typeE2, typeE4)::rest)
	| (TyId(x), TyId(x))::rest ->
	| (TyId(x), type)::rest ->
		if occurCheck type x then raise (FalhaNoUnify "Unify falhou") else
	| (type, TyId(x))::rest ->
		if occurCheck type x then raise (FalhaNoUnify "Unify falhou") else
	| (_,_) -> raise (FalhaNoUnify "Unify falhou")
*)
