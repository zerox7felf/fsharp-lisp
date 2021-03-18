// main.fs

namespace FsLisp
open System.IO
open Types
open Parser
open Eval

module FsLisp = 
    let rec astToString (astList: AstNode list) depth =
        match astList with
        | head :: tail ->
            match head with
            | Empty -> printf "%s%s\n" (String.replicate depth " ") "Empty" 
            | Value token -> 
                match token with
                | Ident ident -> printf "%s%s\n" (String.replicate depth " ") ident
                | Const(Bool boolean) -> printf "%s%s\n" (String.replicate depth " ") (if boolean then "true" else "false")
                | Const(Int integer) -> printf "%s%s\n" (String.replicate depth " ") (string integer)
                | Const(Void) -> printf "Void!\n"
            | Node(identifier, nodes) ->
                printf "%s%s\n" (String.replicate depth " ") identifier
                astToString nodes (depth + 4)
            | Seq list ->
                astToString list depth
            astToString tail depth
        | _ -> ()


    [<EntryPoint>]
    let main args =
        if args.Length <> 1 then
            printfn "Usage: Main.exe <script file>"
            1
        else
            let input = File.ReadAllText(args.[0])

            printfn "Expression:\n%A" input

            let result = parse input stdSymbols
            printfn "Return value from parse:\n%A" result

            match result with
            | (Error msg, (index, line, col)) ->
                printfn "Ajdå."
            | (ErrSome (ast, _), (index, line, col)) ->
                printfn "Generated Ast:\n%A" ast
                astToString [ast] 0
                match eval stdSymbols ast with
                | Error msg -> printfn "Ajdå. \n%A" msg
                | ErrSome token ->
                    printf "Return: "
                    match token with
                    | Ident ident -> printfn "%A" token
                    | Const(Bool boolean) -> printfn "%A" boolean
                    | Const(Int integer) -> printfn "%A" integer
                    | Const(Void) -> printfn "Void!"
            0
