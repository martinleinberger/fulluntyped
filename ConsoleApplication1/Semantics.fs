module Semantics

open AST

let rec isNumericVal = function 
    | _ -> false

let rec isVal = function
    | TmTrue -> true
    | TmFalse -> true
    | t when isNumericVal t -> true
    | TmAbs (_,_) -> true
    | _ -> false