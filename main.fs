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

    let result = parser "tjena 12 true       32 \n87)" (SymbolTable (Map.ofList [])) [] ""
    printfn "%A" result
    match result with
    | Error msg ->
        printfn "Ajdå."
    | ErrSome (ast, _) ->
        printfn "%A" ast
        astToString [ast] 0
