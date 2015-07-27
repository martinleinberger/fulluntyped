module AST

[<StructuredFormatDisplay("{AsString}")>]
type Term = 
    | TmTrue
    | TmFalse
    | TmIf of Term * Term * Term
    | TmVar of int
    | TmAbs of string * Term
    | TmApp of Term * Term
    with
        override self.ToString() = 
            match self with
            | TmTrue -> "true"
            | TmFalse -> "false"
            | TmIf (t1,t2,t3) -> sprintf "if %A \n then %A \n else %A" t1 t2 t3
            | TmVar s -> string s
            | TmAbs (s,t) -> sprintf "(λ%s.%A)" s t
            | TmApp (t1,t2) -> sprintf "(%A %A)" t1 t2
        member self.AsString = self.ToString()
            

type Binding = 
    | NameBinding

type Context = (string * Binding) list
// Ok, for some reason I'm not able to define this directly in the parser :/
type Toplevel = Context -> Term

let emptyContext : Context = []

let addBinding ctx name binding = (name,binding) :: ctx
let addName ctx name = addBinding ctx name NameBinding

let rec name2Index ctx name = 
    match ctx with
    | [] -> failwith <| sprintf "Unbound variable: %A" name
    | (x,_)::rest ->
        if name = x then 0
        else 1 + name2Index rest name


