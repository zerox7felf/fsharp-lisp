namespace FsLisp
open Types
open System

module Parser = 
    // Parts that will be parsed, and assembled into an AST
    type ExpressionPart =
    | AstNode of AstNode    // Subexpression
    | Token of Token        // Indentifier or Constant

    let rec parser input (SymbolTable symbolTable) (parsedSubexpressions: ExpressionPart list) currWord : Error<(AstNode * SymbolTable)> = 
        // Skip over areas of whitespace
        let rec parseWhiteSpace (input: string) symbolTable parsedSubexpressions =
            match input.[0] with
            | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                parseWhiteSpace input.[1..] symbolTable parsedSubexpressions
            | _ ->
                parser input symbolTable parsedSubexpressions ""

        // Parse a number and add it to the list of parsedSubexpressions, which is then passed on for parsing the rest of the expression
        // TODO: parse the right way around
        let rec parseNumber (input: string) number pos symbolTable parsedSubexpressions : Error<(AstNode * SymbolTable)> =
            match input.[0] with
            | _ when Char.IsNumber(Convert.ToChar(input.[0])) ->
                parseNumber input.[1..] (number + (int(Char.GetNumericValue(input.[0]))) * (pown 10 pos)) (pos + 1) symbolTable parsedSubexpressions
            | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                parser input.[1..] symbolTable (parsedSubexpressions@[AstNode(Value(Int number))]) ""
            | ')' -> 
                parser input symbolTable (parsedSubexpressions@[AstNode(Value(Int number))]) ""
            | _ ->
                Error ("Unexpected character " + (string input.[0]) + " in number")

        // Turn string into token
        let makeToken word : Error<Token> = 
            ErrSome(
                match word with
                | "true" -> Const(Bool true)
                | "false" -> Const(Bool false)
                | _ -> Ident word
            )

        // Turn list of ExpressionParts into AstNode
        let rec expressionListToAst parsedSubexpressions currIdent currArgs : Error<AstNode> = 
            match parsedSubexpressions with
            | head :: tail ->
                match head with
                | AstNode node -> 
                    expressionListToAst tail currIdent (currArgs@[node])
                    // TODO: handle sequential expressions
                | Token token ->
                    match token with
                    | Const constant ->
                        expressionListToAst tail currIdent (currArgs@[Value constant])
                    | Ident identifier when currIdent = "" && currArgs.Length = 0 ->
                        expressionListToAst tail identifier []
                    | Ident identifier ->
                        Error "Unexpected identifier in function arguments"
            | _ when currIdent = "" && currArgs.Length = 0 ->
                Error "Empty expression"
            | _ when currIdent = "" && currArgs.Length = 1 ->
                ErrSome(currArgs.[0])
            | _ ->
                ErrSome(Node(currIdent, currArgs))

        printfn "%s" input

        match input.[0] with
        | _ when Char.IsNumber(Convert.ToChar(input.[0])) && currWord = "" -> // currWord = "" allows for other tokens to contain numerals, so long as they do not begin with them
            // Numbers
            parseNumber input 0 0 (SymbolTable symbolTable) parsedSubexpressions
        | '(' ->
            // Subexpression
            // PARSE SUBEXPRESSION AND SHOVE INTO PARSEDSUBEXPRESSIONS
            if currWord = "" then
                let subexpression = parser input.[1..] (SymbolTable symbolTable) [] ""
                // SKIP OVER SUBEXPR.
                // KEEP PARSING WITH NEW PARSEDSUBEXPRESSIONS AFTER CORRECT CLOSE PAREN.
            else
                Error "Unexpected expression start in the middle of token"

        | ')' ->
            // End of expression
            match expressionListToAst parsedSubexpressions "" [] with
            | ErrSome astNode -> ErrSome(astNode, SymbolTable symbolTable)
            | Error msg -> Error msg
        | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
            // End of word
            if currWord = "" then
                parseWhiteSpace input.[1..] (SymbolTable symbolTable) parsedSubexpressions
            else 
                match makeToken currWord with
                | ErrSome token -> parseWhiteSpace input.[1..] (SymbolTable symbolTable) (parsedSubexpressions@[Token token])
                | Error msg -> Error msg
        | _ ->
            // Other token (either keyword or identifier)
            parser input.[1..] (SymbolTable symbolTable) parsedSubexpressions (currWord + (string input.[0]))
