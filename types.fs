namespace FsLisp

module rec Types =
    type Error<'a> =
    | ErrSome of 'a
    | Error of string

    let warn msg (_, line, col) =
        printfn "Warning: %A (line %d, col %d)" msg (line+1) (col+1)

    type Const = 
    | Bool of bool
    | Int of int
    | Void
    | Str of string

    type Ident = string

    type ValueToken = 
    | Const of Const
    | Func of SymbolTableFunc
    
    type Token =
    | Ident of Ident
    | ValueToken of ValueToken

    type AstNode =
    | Empty
    | Value of Token
    | Node of (AstNode * AstNode list)

    type SymbolTableFunc = AstNode list -> SymbolTable -> Error<ValueToken * SymbolTable>
    and SymbolTable = SymbolTable of Map<Ident, (SymbolTableFunc * bool)>
