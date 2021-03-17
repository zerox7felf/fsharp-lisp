namespace FsLisp

module FsLisp = 

    type Error<'a> =
    | Some of 'a
    | Error of string

    type Const = 
    | Bool of bool
    | Int of int

    type Ident = string
    
    type Token =
    | Const of Const
    | Ident of Ident

    type SymbolTableFunc = Token list -> SymbolTable -> Error<Token>
    and SymbolTable = SymbolTable of Map<Ident, SymbolTableFunc>

    type AstNode =
    | Empty
    | Value of Const
    | Node of (Ident * AstNode list)
    | Seq of AstNode list
