module AST

[<StructuredFormatDisplay("{AsString}")>]
type Term = 
    | TmTrue
    | TmFalse
    | TmIf of Term * Term * Term
    | TmVar of int
    | TmAbs of string * Term
    | TmApp of Term * Term
    | TmRecord of (string * Term) list
    | TmString of string
    | TmLet of string * Term * Term
    
    override self.ToString() = 
        match self with
        | TmTrue -> "true"
        | TmFalse -> "false"
        | TmIf(t1, t2, t3) -> 
            sprintf "if %A \n then %A \n else %A" t1 t2 t3
        | TmVar i -> string i
        | TmAbs(x, t) -> sprintf "(λ%s.%A)" x t
        | TmApp(t1, t2) -> sprintf "(%A %A)" t1 t2
        | TmRecord fields -> sprintf "{ %A }" fields
        | TmString s -> s
        | TmLet (x, t1, t2) -> sprintf "let %s=%A in \n %A" x t1 t2
    
    member self.AsString = self.ToString()

type Binding = 
    | NameBinding

type Context = (string * Binding) list

// Ok, for some reason I'm not able to define 
// this directly in the parser :/
type Toplevel = Context -> Term

let emptyContext : Context = []
let addBinding ctx name binding = (name, binding) :: ctx
let addName ctx name = addBinding ctx name NameBinding

let rec name2Index ctx name = 
    match ctx with
    | [] -> failwith <| sprintf "Unbound variable: %A" name
    | (x, _) :: rest -> 
        if name = x then 0
        else 1 + name2Index rest name

let tmmap onvar c t =
  let rec walk c = 
    function
    | TmTrue as t -> t
    | TmFalse as t -> t
    | TmIf (t1, t2, t3) -> TmIf (walk c t1, walk c t2, walk c t3)
    | TmVar x -> onvar c x
    | TmAbs (x, t2) -> TmAbs (x, walk (c + 1) t2)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | TmRecord fields ->
        fields
        |> List.map (fun (label, field) -> label, walk c field)
        |> TmRecord
    | TmString _ as t -> t
    | TmLet (x, t1, t2) -> TmLet (x, walk c t1, walk (c + 1) t2)
  in walk c t
  
let termShiftAbove d c t =
  tmmap
    (fun c x ->
       if x >= c then TmVar (x + d) 
       else TmVar x)
    c t
  
let termShift d t = termShiftAbove d 0 t
  
let termSubst j s t =
  tmmap
    (fun c x -> 
        if x = (j + c) then termShift c s 
        else TmVar x)
    0 t

// term that is being substituted for the bound variable is
// first shifted up by one, then the substitution is made
// and then the whole result is shifted down by one to 
// account for the fact that the bound variable has been 
// used up
let termSubstTop s t = termShift -1 (termSubst 0 (termShift 1 s) t)


// Mathematical definitions from the book:
//let rec termShiftAbove c d = 
//    function 
//    | TmTrue as t -> t
//    | TmFalse as t -> t
//    | TmIf(t1, t2, t3) -> TmIf(termShiftAbove c d t1, termShiftAbove c d t2, termShiftAbove c d t3)
//    | TmVar k -> 
//        if k < c then TmVar k
//        else TmVar(k + d)
//    | TmAbs(x, t) -> TmAbs(x, termShiftAbove (c + 1) d t)
//    | TmApp(t1, t2) -> TmApp(termShiftAbove c d t1, termShiftAbove c d t2)
//    | TmString _ as t -> t
//    | TmLet (x, t1, t2) -> 
//        TmLet (x, termShiftAbove c d t1, termShiftAbove (c+1) d t2)
//
//let rec termShift d t = termShiftAbove 0 d t
//
//// [j->s]t replace variable (j:int) with term s 
//// in term t
//let rec termSubst j s = 
//    function 
//    | TmTrue as t -> t
//    | TmFalse as t -> t
//    | TmIf(t1, t2, t3) -> TmIf(termSubst j s t1, termSubst j s t2, termSubst j s t3)
//    | TmVar(k) -> 
//        if k = j then s
//        else TmVar k
//    | TmAbs(x, t) -> TmAbs(x, termSubst (j + 1) (termShift 1 s) t)
//    | TmApp(t1, t2) -> TmApp(termSubst j s t1, termSubst j s t2)
//    | TmString _ as t -> t
//    | TmLet (x, t1, t2) ->
//        TmLet (x, termSubst j s t1, termSubst (j+1) s t2)

