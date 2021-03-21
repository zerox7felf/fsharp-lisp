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
                | ValueToken token -> printf "%s%s\n" (String.replicate depth " ") (token.ToString())
            | Node(identifier, nodes) ->
                astToString [identifier] depth
                astToString nodes (depth + 4)
            astToString tail depth
        | _ -> ()


    [<EntryPoint>]
    let main args =
        let files = Array.filter (fun (arg: string) -> arg.[0] <> '-') args
        let options = Array.filter (fun (arg: string) -> arg.[0] = '-') args
        let argSet =
            Set.ofList (
                List.fold (
                    fun newArgs (arg:string) ->
                        if arg.Length >= 2 && arg.[0] = '-' && arg.[1] <> '-' then
                            List.append newArgs (
                                List.fold (
                                    fun strArr ch -> (string ch)::strArr
                                ) [] (Seq.toList arg.[1..])
                            )
                        else if arg.Length >= 3 then
                            (arg.[2..] :: newArgs)
                        else newArgs
                ) [] (Array.toList options)
            )
        if (argSet.Contains("h")) || (argSet.Contains("help")) then
            printfn "Usage: FsLisp.exe [options] <script files>"
            printfn "   or: FsLisp.exe [options]\n"
            printfn "Options:"
            printfn "   -h, --help      :   This information"
            printfn "   -a, --ast       :   Show parsed ast"
            printfn "   -d, --debug-ast :   Show raw data from parser"
            1
        else if files.Length = 0 then
            printfn "FsLisp REPL"
            printfn "Type FsLisp.exe -h for help"
            let rec loop symbols = 
                printf "> "

                let input =
                    let line = System.Console.ReadLine()
                    if line = "" then ""
                    else line.[..(line.Length-1)]

                match parse input symbols with
                | (Error(msg), (_, line, col)) ->
                    printfn "Error: %s\n at line %d, col %d" msg (line+1) (col+1)
                    loop symbols
                | (ErrSome(ast, genSymbols), _) ->
                    if argSet.Contains("a") || argSet.Contains("ast") then
                        printfn "Ast:"
                        astToString [ast] 0
                    if argSet.Contains("d") || argSet.Contains("debug-ast") then
                        printfn "Ast data: %A" ast
                    match eval genSymbols ast with
                    | Error(msg) ->
                        printfn "Error: %s" msg
                        loop symbols
                    | ErrSome(token, genSymbols) ->
                        printfn "%s" (token.ToString())
                        loop genSymbols
            loop stdSymbols
        else
            Array.iter (fun file ->
                let input = File.ReadAllText(file)
                match parse input stdSymbols with
                | (Error msg, (index, line, col)) ->
                    printfn "Error: %s\n at line %d, col %d" msg (line+1) (col+1)
                | (ErrSome (ast, _), (index, line, col)) ->
                    if argSet.Contains("a") || argSet.Contains("ast") then
                        printfn "Ast:"
                        astToString [ast] 0
                    if argSet.Contains("d") || argSet.Contains("debug-ast") then
                        printfn "Ast data: %A" ast
                    match eval stdSymbols ast with
                    | Error msg -> printfn "Error: %s" msg
                    | ErrSome(token, _) ->
                        printfn "%s" (token.ToString())
            ) files
            0
