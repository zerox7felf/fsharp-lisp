namespace FsLisp

module Types =
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

    type AstNode =
    | Empty
    | Value of Const
    | Node of (Ident * AstNode list)
    | Seq of AstNode list

    type SymbolTableFunc = AstNode list -> SymbolTable -> Error<Const>
    and SymbolTable = SymbolTable of Map<Ident, SymbolTableFunc>
