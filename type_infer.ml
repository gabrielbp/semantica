#use "sintaxe.ml"
#use "resultados.ml"

exception TypeNotFound of string

type tipo = TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo | TyId of string | TyPair of tipo * tipo

let count = ref (-1);;
let newVar (c : int ref) = begin incr c; "X" ^ string_of_int (!c) end

let rec get_constraints (envmnt : env) (e : expr) = (
	match(e) with
		  Ncte(n) -> (TyInt, [])
		| Bcte(b) -> (TyBool, [])
		| Binop(op, e1, e2) -> ( match(op) with
			  Sum -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Sub -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Mult -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| Div -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyInt, List.concat[newConstraint; constraintE1; constraintE2])
			| And -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyBool); (typeE2, TyBool)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Or -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyBool); (typeE2, TyBool)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Eq -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Df -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Lt -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Le -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Gt -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
			| Ge -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let (typeE2, constraintE2) = get_constraints envmnt e2 in
							let newConstraint = [(typeE1, TyInt); (typeE2, TyInt)] in
								(TyBool, List.concat[newConstraint; constraintE1; constraintE2])
		)
		| Unop(op, e1) -> ( match(op) with
			  Not -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let newConstraint = [(typeE1, TyBool)] in
							(TyBool, List.concat[newConstraint; constraintE1])
		)
		| Pair(e1, e2) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
							let (typeE2, constraintE2) = get_constraints envmnt e2 in
								(TyPair(typeE1, typeE2), List.concat[constraintE1; constraintE2])
		| If(e1, e2, e3) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
								let (typeE2, constraintE2) = get_constraints envmnt e2 in
									let (typeE3, constraintE3) = get_constraints envmnt e3 in
										let newConstraint = [(typeE1, TyBool); (typeE2, typeE3)] in
											(typeE2, List.concat[newConstraint; constraintE1; constraintE2; constraintE3])
		| App(e1, e2) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
							let (typeE2, constraintE2) = get_constraints envmnt e2 in
								let newType = newVar count in
									let newConstraint = [(typeE1, TyFn(typeE2, TyId(newType)))] in
										(TyId(newType), List.concat[newConstraint; constraintE1; constraintE2])
		| Nil -> let newType = newVar count in (TyList(TyId(newType)), [])
		| Cons(e1, e2) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
							let (typeE2, constraintE2) = get_constraints envmnt e2 in
								let newConstraint = [(TyList(typeE1), typeE2)] in
									(typeE2, List.concat[newConstraint; constraintE1; constraintE2])
		| Hd(e1) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let newType = newVar count in
							let newConstraint = [(typeE1, TyList(TyId(newType)))] in
								(TyId(newType), List.concat[newConstraint; constraintE1])
		| Tl(e1) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let newType = newVar count in
							let newConstraint = [(typeE1, TyList(TyId(newType)))] in
								(TyId(newType), List.concat[newConstraint; constraintE1])
		| IsEmpty(e1) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
						let newType = newVar count in
							let newConstraint = [(typeE1, TyList(TyId(newType)))] in
								(TyBool, List.concat[newConstraint; constraintE1])
		| Raise -> let newType = newVar count in (TyId(newType), [])
		| Try(e1, e2) -> let (typeE1, constraintE1) = get_constraints envmnt e1 in
							let (typeE2, constraintE2) = get_constraints envmnt e2 in
								let newConstraint = [(typeE1, typeE2)] in
									(typeE1, List.concat[newConstraint; constraintE1; constraintE2])
)

let rec unify constraints = match(constraints) with
	  [] -> []
	| (TyInt, TyInt)::rest -> unify rest
	| (TyBool, TyBool)::rest -> unify rest
	| (TyFn(typeE1, typeE2), TyFn(typeE3, typeE4))::rest -> unify ((typeE1, typeE3)::(typeE2, typeE4)::rest)
	| (TyPair(typeE1, typeE2), TyPair(typeE3, typeE4))::rest -> unify ((typeE1, typeE3)::(typeE2, typeE4)::rest)
	| (TyList(typeE1), TyList(typeE2))::rest -> unify ((typeE1, typeE2)::rest)