// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open AST
open System.IO
open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main argv = 
    let parseProgram text = 
        let lexbuf = LexBuffer<char>.FromString text
        Parser.start Lexer.tokenize lexbuf emptyContext
        



    parseProgram "λx.x" |> printfn "%A"
    parseProgram "if true then true else false" |> printfn "%A"
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
