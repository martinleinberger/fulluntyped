module Semantics

open AST

exception NoRuleApplies

let rec isNumericVal = function 
    | _ -> false

let rec isVal = function
    | TmTrue -> true
    | TmFalse -> true
    | TmString _ -> true
    | t when isNumericVal t -> true
    | TmAbs (_,_) -> true
    | TmRecord fields -> List.forall (fun (l,ti) -> isVal ti) fields
    | _ -> false

let rec eval1 = function
    | TmIf (TmTrue, t2, _) -> t2
    | TmIf (TmFalse, _, t3) -> t3
    | TmIf (t1, t2, t3) -> TmIf (eval1 t1, t2, t3)
    | TmApp (TmAbs(x,t), v) when isVal v -> termSubstTop v t
    | TmApp (v,t) when isVal v -> TmApp (v, eval1 t)
    | TmApp (t1, t2) -> TmApp (eval1 t1, t2)
    | TmRecord fields ->
        let rec evalField = function
            | [] -> raise NoRuleApplies
            | (l,v)::rest when isVal v -> (l,v) :: evalField rest
            | (l,t)::rest -> (l, eval1 t) :: rest
        evalField fields
        |> TmRecord
    | TmLet (x, v, t) when isVal v -> termSubstTop v t
    | TmLet (x, t1, t2) -> TmLet (x, eval1 t1, t2)
    | _ -> raise NoRuleApplies

let rec eval t = 
    try
        let t' = eval1 t
        eval t'
    with NoRuleApplies -> t

let evaluateProgram p = 
    let p' = eval p
    p'
    //if isVal p' then p'
    //else failwith 
    //    <| sprintf "Runtime error: Evaluation stuck on %A" p