// eval.fs
// desc: evaluates abstract syntax tree

namespace FsLisp

open Types

module Eval =

    let areSame = LanguagePrimitives.PhysicalEquality // Equality comparison for functions

    let rec eval (SymbolTable symbols) (ast: AstNode) : Error<Token * SymbolTable> =
        match ast with
        | Empty ->
            ErrSome(Const(Void), (SymbolTable(symbols)))
        | Value value ->
            ErrSome(value, (SymbolTable(symbols)))
        | Node (identExpr, nodeList) ->
            match eval (SymbolTable symbols) identExpr with
            | ErrSome(Ident(ident), (SymbolTable symbols)) -> 
                match symbols.TryFind (ident) with
                | Some symbol ->
                    // printf "Symbol ('%s'): %A\n" ident symbol
                    match symbol (nodeList) (SymbolTable symbols) with
                    | ErrSome result ->
                        ErrSome(result)
                    | Error err ->
                        Error(err)
                | None ->
                    Error("Symbol " + ident + " was not found")
            | Error(msg) -> Error(msg)
            | ErrSome(Const(constant), _) ->
                Error(
                    "Attempt to use " + (
                        match constant with
                        | Bool boolean -> "boolean (" + (if boolean then "true" else "false") + ")"
                        | Int integer -> "integer (" + (string integer) + ")"
                        | Void -> "void"
                    ) + " as function identifier"
                )
        //| Seq seq ->
        //    let rec iterateSequence (SymbolTable symbols) (seq: AstNode list) (lastValue: Error<Token>) : Error<Token> =
        //        match seq with
        //        | head :: tail ->
        //            iterateSequence (SymbolTable symbols) (tail) (eval (SymbolTable(symbols)) (head))
        //        | _ ->
        //            lastValue
        //    iterateSequence (SymbolTable symbols) (seq) (ErrSome(Const(Void)))

    let opArith (ast: AstNode list) (table: SymbolTable) (op) : Error<Token * SymbolTable> =
        if not (ast.Length = 2) then
            Error "Invalid number of arguments in arithmetic operation"
        else
            match eval table ast.[0] with
            | ErrSome(left, _) ->
                match eval table ast.[1] with
                | ErrSome(right, _) ->
                    match (left, right) with
                    | (Const(Int(left)), Const(Int(right))) ->
                        ErrSome(Const(Int(op left right)), table)
                    | _ ->
                        Error "Invalid types in arithmetic operation"
                | Error(msg) -> Error(msg)
            | Error(msg) -> Error(msg)

    let relationalArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 2) then
            Error "Invalid number of arguments in relational arithmetic operation"
        else
            match eval table ast.[0] with
            | ErrSome(left, _) ->
                match eval table ast.[1] with
                | ErrSome(right, _) ->
                    match (left, right) with
                    | (Const(Bool(left)), Const(Bool(right))) ->
                        ErrSome(Const(Bool(op left right)), table)
                    | _ ->
                        Error "Invalid types in relational arithmetic operation"
                | Error(msg) -> Error(msg)
            | Error(msg) -> Error(msg)

    let stdSymbols : SymbolTable =
        SymbolTable(
            Map.ofList [
                (
                    "+",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (+)
                    )
                );
                (
                    "-",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (-)
                    )
                );
                (
                    "*",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (*)
                    )
                );
                (
                    "/",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (/)
                    )
                );
                (
                    "==",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        relationalArith ast table (=)
                    )
                );
                (
                    "<",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        relationalArith ast table (<)
                    )
                );
                (
                    ">",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        relationalArith ast table (>)
                    )
                );
                (
                    // \--if
                    //    \-- (cond)
                    //    \-- (true-body)
                    //    \-- (false-body)  -- optional
                    "if",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        let num_args = ast.Length
                        if num_args < 2 then
                            Error "Invalid number of arguments in 'if' function"
                        else
                            match eval table ast.[0] with
                            | ErrSome(value, table) ->
                                match value with
                                | Const(Bool(true)) ->
                                    eval table ast.[1]
                                | Const(Bool(false)) ->
                                    if num_args = 3 then
                                        eval table ast.[2]
                                    else
                                        ErrSome(value, table)
                                | _ ->
                                    Error "Invalid type in 'if' function condition"
                            | Error(err) ->
                                Error err
                    )
                );
                (
                    "define",
                    (fun (ast: AstNode list) (table: SymbolTable) -> Error "Not implemented!")
                );
                (
                    "print",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        let num_args = ast.Length
                        if num_args <> 1 then
                            Error "Invalid number of arguments"
                        else
                            match (eval table ast.[0]) with
                            | ErrSome(result, table) ->
                                match result with
                                | Const(Bool(value)) ->
                                    printf "%A\n" value
                                | Const(Int(value)) ->
                                    printf "%A\n" value
                                | Const(Void) ->
                                    printf "Void\n"
                                | Ident value ->
                                    printf "%A\n" value
                                ErrSome(result, table)
                            | Error(err) ->
                                Error(err)
                    )
                );
                //(
                //    "->", // run arguments consecutively. name WIP
                //    (fun (ast: AstNode list) (table: SymbolTable) ->
                //        let rec execNext expressions table =
                //            match expressions with
                //            | head :: tail ->
                //                
                //            | _ -> table
                //    )
                //);
                (
                    "test",
                    (fun (ast: AstNode list) (table: SymbolTable) -> Error "Not implemented!")
                );
            ]
        )

    let evalRun() =
        let ast =
            Node(
                Value(Ident("if")),
                [
                    Node(
                        Value(Ident("if")),
                        [
                            Value(Const(Bool(false)));
                            Value(Const(Bool(true)));
                        ]
                    );
                    Value(Const(Int(5)));
                    Value(Const(Int(7)));
                ]
            )

        let result = eval stdSymbols ast
        match result with
        | Error(err) ->
            printf "Runtime error: %s\n" err
        | _ ->
            printf "Result: %A\n" result
