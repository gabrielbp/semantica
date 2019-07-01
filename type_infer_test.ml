#use "type_infer.ml"

let num_test = get_constraints [] (Ncte(1));; (* must return (TyInt, []) *)