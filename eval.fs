// eval.fs
// desc: evaluates abstract syntax tree

namespace FsLisp

open Types

module Eval =

    let areSame = LanguagePrimitives.PhysicalEquality // Equality comparison for functions

    let rec eval (SymbolTable symbols) (ast: AstNode) : Error<Const> =
        match ast with
        | Empty ->
            ErrSome(Void)
        | Value value ->
            ErrSome(value)
        | Node (ident, nodeList) ->
            match symbols.TryFind (ident) with
            | Some symbol ->
                // printf "Symbol ('%s'): %A\n" ident symbol
                match symbol (nodeList) (SymbolTable symbols) with
                | ErrSome result ->
                    ErrSome(result)
                | Error err ->
                    Error(err)
            | None ->
                Error("Symbol ' + ident + ' was not found")
        | Seq seq ->
            let rec iterateSequence (SymbolTable symbols) (seq: AstNode list) (lastValue: Error<Const>) : Error<Const> =
                match seq with
                | head :: tail ->
                    iterateSequence (SymbolTable symbols) (tail) (eval (SymbolTable(symbols)) (head))
                | _ ->
                    lastValue
            iterateSequence (SymbolTable symbols) (seq) (ErrSome(Void))

    let opArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 2) then
            Error "Invalid number of arguments in arithmetic operation"
        else
            let left = eval table ast.[0]
            let right = eval table ast.[1]
            match (left, right) with
            | (ErrSome(Int(left)), ErrSome(Int(right))) ->
                ErrSome(Int(op left right))
            | _ ->
                Error "Invalid types in arithmetic operation"

    let relationalArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 2) then
            Error "Invalid number of arguments in relational arithmetic operation"
        else
            let left = eval table ast.[0]
            let right = eval table ast.[1]
            match (ErrSome(left), ErrSome(right)) with
            | (ErrSome(left), ErrSome(right)) ->
                ErrSome(Bool(op left right))
            | _ ->
                Error "Invalid types in relational arithmetic operation"

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
                            let cond = eval table ast.[0]
                            match ErrSome(cond) with
                            | ErrSome(value) ->
                                match value with
                                | ErrSome(Bool(true)) ->
                                    eval table ast.[1]
                                | ErrSome(Bool(false)) ->
                                    if num_args = 3 then
                                        eval table ast.[2]
                                    else
                                        cond
                                | _ ->
                                    Error "Invalid type in 'if' function condition"
                            | Error(err) ->
                                Error err
                    )
                );
                (
                    "test",
                    (fun (ast: AstNode list) (table: SymbolTable) -> Error "Not implemented!")
                );
                (
                    "print",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        let num_args = ast.Length
                        if num_args < 1 then
                            Error "Invalid number of arguments"
                        else
                            match (eval table ast.[0]) with
                            | ErrSome(result) ->
                                match result with
                                | Bool(value) ->
                                    printf "%A\n" value
                                | Int(value) ->
                                    printf "%A\n" value
                                | Void ->
                                    printf "Void\n"
                                ErrSome(result)
                            | Error(err) ->
                                Error(err)
                    )
                );
            ]
        )

    let evalRun() =
        let ast =
            Node(
                "if",
                [
                    Seq(
                        [
                            Node(
                                "if",
                                [
                                    Seq(
                                        [Value(Bool(false));]
                                    );
                                    Seq(
                                        [Value(Bool(true));]
                                    );
                                ]
                            )
                        ]
                    );
                    Seq(
                        [Value(Int(5));]
                    );
                    Seq(
                        [Value(Int(7));]
                    );
                ]
            )

        let result = eval stdSymbols ast
        match result with
        | Error(err) ->
            printf "Runtime error: %s\n" err
        | _ ->
            printf "Result: %A\n" result
