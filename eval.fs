// eval.fs
// desc: evaluates abstract syntax tree

namespace FsLisp

open Types

module Eval =

    let areSame = LanguagePrimitives.PhysicalEquality // Equality comparison for functions

    let eval (SymbolTable symbols) (ast: AstNode) : Const =
        match ast with
        | Empty ->
            Void
        | Value value ->
            value
        | Node (ident, nodeList) ->
            match symbols.TryFind (ident) with
            | Some symbol ->
                // printf "Symbol ('%s'): %A\n" ident symbol
                match symbol (nodeList) (SymbolTable symbols) with
                | ErrSome result ->
                    // printf "Result: %A\n" result
                    result
                | Error err ->
                    printf "NOK: %s\n" err
                    Void
            | None ->
                printf "Symbol '%s' was not found\n" ident
                Void
        | Seq seq ->
            printf "Seq\n"
            Void

    let opArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 2) then
            Error "Invalid number of arguments in arithmetic operation"
        else
            let left : Const = eval table ast.[0]
            let right : Const = eval table ast.[1]
            match (left, right) with
            | (Int(left), Int(right)) ->
                ErrSome(Int(op left right))
            | _ ->
                Error "Invalid types in arithmetic operation"

    let relationalArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 2) then
            Error "Invalid number of arguments in relational arithmetic operation"
        else
            let left : Const = eval table ast.[0]
            let right : Const = eval table ast.[1]
            match (left, right) with
            | (Int(left), Int(right)) ->
                ErrSome(Bool(op left right))
            | (Bool(left), Bool(right)) when (areSame op (=)) ->
                ErrSome(Bool(left = right))
            | _ ->
                Error "Invalid types in relational arithmetic operation"

    let evalRun =
        let ast =
            Node(
                "<",
                [
                    Value(Int(2));
                    Node(
                        "+",
                        [
                            Value(Int(3));
                            Value(Int(4));
                        ]
                    )
                ]
            )

        let symbols =
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
                        "test",
                        (fun (ast: AstNode list) (table: SymbolTable) -> Error "Not implemented!")
                    );
                ]
            )

        let result = eval symbols ast
        printf "Result: %A\n" result
