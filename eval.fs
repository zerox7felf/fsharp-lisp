// eval.fs
// desc: evaluates abstract syntax tree

namespace FsLisp

open Types

module Eval =
    let rec eval (SymbolTable symbols) (ast: AstNode) : Error<ValueToken * SymbolTable> =
        match ast with
        | Empty -> ErrSome(Const(Void), SymbolTable(symbols))
        | Value value ->
            match value with
            | Ident ident ->
                match symbols.TryFind ident with
                | Some (symbol, _) -> ErrSome(Func(symbol), SymbolTable(symbols))
                | None -> Error("Symbol " + ident + " was not found")
            | ValueToken value -> ErrSome(value, SymbolTable(symbols))
        | Node(identExpr, nodeList) ->
            match eval (SymbolTable symbols) identExpr with
            | Error(msg) -> Error(msg)
            | ErrSome(Const(constant), _) -> Error("Attempt to use " + constant.ToString() + " as function")
            | ErrSome(Func(func), _) -> func nodeList (SymbolTable symbols) 

    let opArith (ast: AstNode list) (table: SymbolTable) (op) : Error<ValueToken * SymbolTable> =
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
                    | (Const(left), Const(right)) ->
                        ErrSome(Const(Bool(op left right)), table)
                    | _ ->
                        Error "Invalid types in relational arithmetic operation"
                | Error(msg) -> Error(msg)
            | Error(msg) -> Error(msg)

    let boolArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 2) then
            Error "Invalid number of arguments in boolean arithmetic operation"
        else
            match eval table ast.[0] with
            | ErrSome(left, _) ->
                match eval table ast.[1] with
                | ErrSome(right, _) ->
                    match (left, right) with
                    | (Const(Bool(left)), Const(Bool(right))) ->
                        ErrSome(Const(Bool(op left right)), table)
                    | _ ->
                        Error "Invalid types in boolean arithmetic operation"
                | Error(msg) -> Error(msg)
            | Error(msg) -> Error(msg)

    let unaryRelArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 1) then
            Error "Invalid number of arguments in unary arithmetic operation"
        else
            match eval table ast.[0] with
            | ErrSome(value, _) ->
                match value with
                | Const(Bool(value)) ->
                    ErrSome(Const(Bool(op value)), table)
                | _ ->
                    Error "Invalid value type in unary operation"
            | Error(msg) -> Error(msg)

    let unaryArith (ast: AstNode list) (table: SymbolTable) (op) =
        if not (ast.Length = 1) then
            Error "Invalid number of arguments in unary arithmetic operation"
        else
            match eval table ast.[0] with
            | ErrSome(value, _) ->
                match value with
                | Const(Int(value)) ->
                    ErrSome(Const(Int(op value)), table)
                | _ ->
                    Error "Invalid value type in unary operation"
            | Error(msg) -> Error(msg)

    let stdSymbols : SymbolTable =
        SymbolTable(
            Map.ofList [
                (
                    "+",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (+)
                    , false)
                );
                (
                    "-",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length = 1 then
                            unaryArith ast table (~-)
                        else
                            opArith ast table (-)
                    , false)
                );
                (
                    "*",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (*)
                    , false)
                );
                (
                    "/",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (/)
                    , false)
                );
                (
                    "==",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        relationalArith ast table (=)
                    , false)
                );
                (
                    "<",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        relationalArith ast table (<)
                    , false)
                );
                (
                    ">",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        relationalArith ast table (>)
                    , false)
                );
                (
                    "and",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        boolArith ast table (&&)
                    , false)
                );
                (
                    "or",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        boolArith ast table (||)
                    , false)
                );
                (
                    "not",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        unaryRelArith ast table (not)
                    , false)
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
                    , false)
                );
                (
                    "print",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        let num_args = ast.Length
                        if num_args < 1 then
                            Error "Invalid number of arguments"
                        else
                            let rec iterateArgs (SymbolTable symbols) (seq: AstNode list) (lastValue: Error<ValueToken * SymbolTable>) : Error<ValueToken * SymbolTable> =
                                match seq with
                                | head :: tail ->
                                    let result = (eval (SymbolTable(symbols)) (head))
                                    match result with
                                    | ErrSome(result, table) ->
                                        printf "%s" (result.ToString())
                                    | Error(err) -> ()
                                    if not (tail.IsEmpty) then
                                        printf " "
                                    iterateArgs (SymbolTable symbols) (tail) (result)
                                | _ ->
                                    lastValue
                            iterateArgs (table) (ast) (ErrSome(Const(Void), table)) |> ignore
                            printf "\n"
                            ErrSome(Const(Void), table)
                    , false)
                );
                (
                    "->", // run arguments consecutively. name WIP
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        let rec loop expressions lastTkn table =
                            match expressions with
                            | head :: tail ->
                                match eval table head with
                                | ErrSome(tkn, table) -> loop tail tkn table
                                | Error(msg) -> Error(msg)
                            | _ -> ErrSome(lastTkn, table)
                        loop ast (Const(Void)) table
                    , false)
                );
                (
                    "$", // construct lambda
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length < 1 then
                            Error("Missing argument for lambda definition")
                        else
                            let rec loop astList argList = ErrSome([])
                                // match astList with
                                // | head :: tail ->
                                //     match eval table head with
                                //     | Error(msg) -> Error(msg)
                                //     | ErrSome(tkn, _) ->
                                //         match tkn with
                                //         | Ident argName ->
                                //             loop tail (argList@[(argName)])
                                //         | _ -> Error("Invalid token for specifying argument name")
                                // | _ -> ErrSome(argList)
                            match loop (if ast.Length > 1 then ast.[..(ast.Length - 2)] else []) [] with
                            | Error(msg) -> Error(msg)
                            | ErrSome(args) ->
                                ErrSome(
                                    Func(
                                        fun (localAst: AstNode list) (SymbolTable ogTable) ->
                                            let rec loop argName argVal (SymbolTable table) =
                                                match (argName, argVal) with
                                                | (name :: nameTail, currVal :: valTail) ->
                                                    loop nameTail valTail (
                                                        SymbolTable(
                                                            table.Add(name, (fun _ tbl ->
                                                                eval (SymbolTable(table)) currVal
                                                            , true))
                                                        )
                                                    )
                                                | _ -> table
                                            match eval (SymbolTable(loop args localAst (SymbolTable ogTable))) ast.[(ast.Length - 1)] with
                                            | Error(msg) -> Error(msg)
                                            | ErrSome(tkn, SymbolTable(table)) -> ErrSome(tkn, SymbolTable(ogTable))
                                    ), table
                                )
                    , false)
                );
                (
                    "let",
                    (fun (ast: AstNode list) (SymbolTable mainTable) -> 
                        // if ast.Length < 2 then
                            Error("Missing arguments for function definition")
                        // else
                        //     match eval (SymbolTable mainTable) ast.[0] with
                        //     | Error(msg) -> Error(msg)
                        //     | ErrSome(tkn, SymbolTable table) ->
                        //         match tkn with
                        //         | Ident name ->
                        //             ErrSome(
                        //                 Const(Void),
                        //                 SymbolTable(
                        //                     table.Add(name, (fun (localAst: AstNode list) (SymbolTable ogTable) ->
                        //                         eval (SymbolTable(table)) ast.[ast.Length-1]
                        //                     , true)) // TODO remove remove flags
                        //                 )
                        //             )
                        //         | _ -> Error("Invalid token for function name")
                    , false)
                );
            ]
        )
