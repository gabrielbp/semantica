type variable = string

type bop = Sum   (* + *)
         | Sub (* - *)
         | Mult  (* x *)
         | Div   (* / *)
         | And   (* and *)
         | Or    (* or *)
         | Eq    (* = *)
         | Df    (* <> *)
         | Lt    (*  < *)
         | Le    (* <= *)
         | Gt    (* > *)
         | Ge    (* >= *)

type uop = Not

type expr = Ncte of int
          | Bcte of bool
          | Binop of bop * expr * expr
          | Unop of uop * expr
          | Pair of expr * expr
          | Fst of expr
          | Snd of expr
          | If of expr * expr * expr
          | Id of variable
          | App of expr * expr
          | Fun of variable * expr
          | Let of variable * expr * expr
          | Lrec of variable *  variable * expr * expr
          | Nil
          | Cons of expr * expr
          | IsEmpty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | Try of expr * expr

type tipo = TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo | TyId of string | TyPair of tipo * tipo

type result = Vnum of int
          | Vbool of bool
          | Vpair of result * result
          | Vnil
          | Vcons of result * result
          | Vclos of variable * expr * env
          | Vrclos of variable * variable * expr * env
          | RRaise
and
   env = (variable * result) list

type   typeEnv = (variable * tipo) list
