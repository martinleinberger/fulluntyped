// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open AST
open Semantics
open System.IO
open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main argv = 
    let parseProgram text = 
        let lexbuf = LexBuffer<char>.FromString text
        Parser.start Lexer.tokenize lexbuf emptyContext
    
    let run text = 
        text
        |> parseProgram
        |> evaluateProgram

    run "(λn.λs.λz. s (n s z)) (λs.λz. z)"
    |> printfn "%A"
    
    run "if true
            then true
            else false"
    |> printfn "%A"
    
    run "\"Hello\""
    |> printfn "%A"

    run "let b=true in (λn.n) b"
    |> printfn "%A"

    run "{ a=true, b=false }"
    |> printfn "%A"
    
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
