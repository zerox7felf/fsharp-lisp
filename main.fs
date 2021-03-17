namespace FsLisp

module FsLisp = 

    type Const = 
    | Bool of bool
    | Int of int
    
    type Token =
    | Const of Const
    | Ident of string

    type Error<'a> =
    | Some of 'a
    | Error of string

    type SymbolTableFunc = Token list -> SymbolTable -> Error<Token>
    and SymbolTable = Map<Ident, SymbolTableFunc>


