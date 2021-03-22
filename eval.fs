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

    let opArith (ast: AstNode list) (table: SymbolTable) (op) (opName): Error<ValueToken * SymbolTable> =
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
                    | (Const(Str(left)), Const(Str(right))) when opName = "+" ->
                        ErrSome(Const(Str(left + right)), table)
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

    let isOfType arg typeName =
        match typeName with
        | "Func" ->
            match arg with
            | Func(_) -> true
            | _ -> false
        | "Bool" ->
            match arg with
            | Const(Bool(_)) -> true
            | _ -> false
        | "Int" ->
            match arg with
            | Const(Int(_)) -> true
            | _ -> false
        | "Str" ->
            match arg with
            | Const(Str(_)) -> true
            | _ -> false
        | "Void" ->
            match arg with
            | Const(Void) -> true
            | _ -> false
        | _ -> false

    let stdSymbols : SymbolTable =
        SymbolTable(
            Map.ofList [
                (   "isFunc",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length > 1 then
                            Error("Too many arguments to isFunc")
                        else
                            match eval table ast.[0] with
                            | Error(msg) -> Error(msg)
                            | ErrSome(result, _) ->
                                ErrSome(Const(Bool(isOfType result "Func")), table)
                    , false)
                );
                (   "isBool",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length > 1 then
                            Error("Too many arguments to isBool")
                        else
                            match eval table ast.[0] with
                            | Error(msg) -> Error(msg)
                            | ErrSome(result, _) ->
                                ErrSome(Const(Bool(isOfType result "Bool")), table)
                    , false)
                );
                (   "isInt",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length > 1 then
                            Error("Too many arguments to isInt")
                        else
                            match eval table ast.[0] with
                            | Error(msg) -> Error(msg)
                            | ErrSome(result, _) ->
                                ErrSome(Const(Bool(isOfType result "Int")), table)
                    , false)
                );
                (   "isStr",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length > 1 then
                            Error("Too many arguments to isStr")
                        else
                            match eval table ast.[0] with
                            | Error(msg) -> Error(msg)
                            | ErrSome(result, _) ->
                                ErrSome(Const(Bool(isOfType result "Str")), table)
                    , false)
                );
                (   "isVoid",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length > 1 then
                            Error("Too many arguments to isVoid")
                        else
                            match eval table ast.[0] with
                            | Error(msg) -> Error(msg)
                            | ErrSome(result, _) ->
                                ErrSome(Const(Bool(isOfType result "Void")), table)
                    , false)
                );
                (
                    "+",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (+) "+"
                    , false)
                );
                (
                    "-",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        if ast.Length = 1 then
                            unaryArith ast table (~-)
                        else
                            opArith ast table (-) "-"
                    , false)
                );
                (
                    "*",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (*) "*"
                    , false)
                );
                (
                    "/",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        opArith ast table (/) "/"
                    , false)
                );
                (
                    "=",
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
                    "if",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        let num_args = ast.Length
                        if num_args < 2 || num_args > 3 then
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
                            let rec iterateArgs (SymbolTable symbols) (seq: AstNode list): Error<ValueToken * SymbolTable> =
                                match seq with
                                | head :: tail ->
                                    let result = (eval (SymbolTable(symbols)) (head))
                                    match result with
                                    | ErrSome(result, table) as wholeRes ->
                                        match result with
                                        | Func(func) -> printf "%A " func
                                        | Const(Bool(cond)) -> printf "%s " (if cond then "true" else "false")
                                        | Const(Int(integer)) -> printf "%d " integer
                                        | Const(Str(str)) -> printf "%s " str
                                        | Const(Void) -> printf "Void " 
                                        iterateArgs (SymbolTable symbols) (tail)
                                    | Error(err) -> Error(err)
                                | _ ->
                                    printf "\n"
                                    ErrSome(Const(Void), table)
                            iterateArgs (table) (ast)
                    , false)
                );
                (
                    "debugPrint",
                    (fun (ast: AstNode list) (table: SymbolTable) ->
                        let num_args = ast.Length
                        if num_args < 1 then
                            Error "Invalid number of arguments"
                        else
                            let rec iterateArgs (SymbolTable symbols) (seq: AstNode list): Error<ValueToken * SymbolTable> =
                                match seq with
                                | head :: tail ->
                                    let result = (eval (SymbolTable(symbols)) (head))
                                    match result with
                                    | ErrSome(result, table) as wholeRes ->
                                        printf "%s" (result.ToString())
                                        if not (tail.IsEmpty) then
                                            printf " "
                                        iterateArgs (SymbolTable symbols) (tail)
                                    | Error(err) -> Error(err)
                                | _ ->
                                    printf "\n"
                                    ErrSome(Const(Void), table)
                            iterateArgs (table) (ast)
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
                            let rec loop astList argList = 
                                match astList with
                                | head :: tail ->
                                    match eval table head with
                                    | Error(msg) -> Error(msg)
                                    | ErrSome(tkn, _) ->
                                        match tkn with
                                        | Const(Str(argName)) ->
                                            loop tail (argList@[(argName)])
                                        | tkn -> Error("Cannot use " + (tkn.ToString()) + " as argument name")
                                | _ -> ErrSome(argList)
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
                                                                match eval (SymbolTable(table)) currVal with
                                                                | Error(msg) -> Error(msg)
                                                                | ErrSome(Func(func), tbl) ->
                                                                    match eval tbl (Node(Value(ValueToken(Func(func))),[])) with
                                                                    | Error(msg) -> Error(msg)
                                                                    | ErrSome(_) as result -> result
                                                                | result -> result
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
                        if ast.Length < 2 then
                            Error("Missing arguments for function definition")
                        else
                            match eval (SymbolTable mainTable) ast.[0] with
                            | Error(message) -> Error(message)
                            | ErrSome(token, SymbolTable table) ->
                                match token with
                                | Const(Str(name)) ->
                                    ErrSome(
                                        Const(Void),
                                        SymbolTable(
                                            mainTable.Add(
                                                name, (fun (localAst: AstNode list) (SymbolTable ogTable) ->
                                                    match eval (SymbolTable(ogTable)) ast.[ast.Length - 1] with
                                                    | Error(msg) -> Error(msg)
                                                    | ErrSome(Func(func), _) ->
                                                        match eval (SymbolTable(ogTable)) (Node(Value(ValueToken(Func(func))), localAst)) with
                                                        | Error(msg) -> Error(msg)
                                                        | ErrSome(result, _) -> ErrSome(result, SymbolTable(ogTable))
                                                    | ErrSome(result, _) -> ErrSome(result, SymbolTable(ogTable))
                                                , true)
                                            )
                                        )
                                    )
                                | _ ->
                                    Error("Invalid token for function name")
                    , false)
                );
            ]
        )
