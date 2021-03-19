namespace FsLisp

module rec Types =
    type Error<'a> =
    | ErrSome of 'a
    | Error of string

    type Const = 
    | Bool of bool
    | Int of int
    | Void

    type Ident = string
    
    type Token =
    | Const of Const
    | Ident of Ident
    | Func of SymbolTableFunc

    type AstNode =
    | Empty
    | Value of Token
    | Node of (AstNode * AstNode list)
    //| Seq of AstNode list

    type SymbolTableFunc = AstNode list -> SymbolTable -> Error<Token * SymbolTable>
    and SymbolTable = SymbolTable of Map<Ident, (SymbolTableFunc * bool)>
