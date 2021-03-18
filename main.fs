namespace FsLisp
open Types
open Parser

module FsLisp = 
    let rec astToString (astList: AstNode list) depth =
        match astList with
        | head :: tail ->
            match head with
            | Empty -> printf "%s%s\n" (String.replicate depth " ") "Empty" 
            | Value constant -> 
                match constant with
                | Bool boolean -> printf "%s%s\n" (String.replicate depth " ") (if boolean then "true" else "false")
                | Int integer -> printf "%s%s\n" (String.replicate depth " ") (string integer)
            | Node(identifier, nodes) ->
                printf "%s%s\n" (String.replicate depth " ") identifier
                astToString nodes (depth + 4)
            | Seq list ->
                astToString list depth
            astToString tail depth
        | _ -> ()

    let input = "(\n\ttjena 1281273 true (\n\t\tflabb 12\n\t)\n)" 
    printfn "%A" input
    let result = parse input (SymbolTable (Map.ofList []))
    printfn "%A" result
    match result with
    | (Error msg, (index, line, col)) ->
        printfn "Ajdå."
    | (ErrSome (ast, _), (index, line, col)) ->
        printfn "%A" ast
        astToString [ast] 0
