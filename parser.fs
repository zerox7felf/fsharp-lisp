namespace FsLisp
open Types
open System

module Parser = 
    // Parts that will be parsed, and assembled into an AST
    type ExpressionPart =
    | AstNode of AstNode    // Subexpression
    | Token of Token        // Indentifier or Constant

    type ParserPosition = (int * int * int) // input index * line nr * column nr

    let rec parser
        input ((index, line, col): ParserPosition) (SymbolTable symbolTable)
        (parsedSubexpressions: ExpressionPart list) currWord : (Error<(AstNode * SymbolTable)> * ParserPosition) =

        // Skip over areas of whitespace
        let rec parseWhiteSpace (input: string) ((index, line, col): ParserPosition) symbolTable parsedSubexpressions =
            if input = "" then
                parser input (index, line, col) symbolTable parsedSubexpressions ""
            else
                match input.[0] with
                | '\n' ->
                    parseWhiteSpace input.[1..] (index+1, line+1, 0) symbolTable parsedSubexpressions
                | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                    parseWhiteSpace input.[1..] (index+1, line, col+1) symbolTable parsedSubexpressions
                | _ ->
                    parser input (index, line, col) symbolTable parsedSubexpressions ""

        // Build a number from an inverted list of number-characters
        let rec buildNumber number numberBuf pos =
            match numberBuf with
            | head :: tail ->
                buildNumber (number + (int(Char.GetNumericValue(head))) * (pown 10 pos)) tail (pos + 1)
            | _ -> number

        // Parse a number and add it to the list of parsedSubexpressions, which is then passed on for parsing the rest of the expression
        // TODO: parse the right way around
        let rec parseNumber (input: string) ((index, line, col): ParserPosition) numberBuf symbolTable parsedSubexpressions =
            if input = "" then
                parser input (index, line, col) symbolTable (parsedSubexpressions@[AstNode(Value(Int (buildNumber 0 numberBuf 0)))]) ""
            else 
                match input.[0] with
                | _ when Char.IsNumber(Convert.ToChar(input.[0])) ->
                     parseNumber
                         input.[1..] (index+1, line, col+1)
                         (input.[0] :: numberBuf)
                         symbolTable parsedSubexpressions
                    // parseNumber
                    //     input.[1..] (index+1, line, col+1)
                    //     (number + (int(Char.GetNumericValue(input.[0]))) * (pown 10 pos)) (pos + 1)
                    //     symbolTable parsedSubexpressions
                | ')'
                | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                    parser input (index, line, col) symbolTable (parsedSubexpressions@[AstNode(Value(Int (buildNumber 0 numberBuf 0)))]) ""
                | _ ->
                    (Error ("Unexpected character " + (string input.[0]) + " in number"), (index, line, col))

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

        printfn "%A" (input.Replace("\n", "\\n"))
        if input = "" then
            (Error "Unexpected end of file", (index, line, col))
        else
            match input.[0] with
            | _ when Char.IsNumber(Convert.ToChar(input.[0])) && currWord = "" ->
                // ^- && currWord = "" allows for other tokens to contain numerals, so long as they do not begin with them
                // Numbers
                parseNumber input (index, line, col) [] (SymbolTable symbolTable) parsedSubexpressions
            | '(' ->
                // Subexpression
                if currWord = "" then
                    match parser input.[1..] (index+1, line, col+1) (SymbolTable symbolTable) [] "" with
                    | (Error msg, (newIndex, newLine, newCol)) -> (Error msg, (newIndex, line, col))
                    | (ErrSome (astNode, newSymbolTable), (newIndex, newLine, newCol)) ->
                        if (1+newIndex-index) > input.Length then
                            (Error "Unexpected end of file", (index, line, col))
                        else
                            parser input.[(1+newIndex-index)..] (newIndex+1, newLine, newCol) newSymbolTable (parsedSubexpressions@[AstNode astNode]) ""
                else
                    (Error "Unexpected expression start in the middle of token", (index, line, col))

            | ')' ->
                // End of expression
                match expressionListToAst parsedSubexpressions "" [] with
                | ErrSome astNode -> (ErrSome(astNode, SymbolTable symbolTable), (index, line, col))
                | Error msg -> (Error msg, (index, line, col))
            | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                // End of word
                if currWord = "" then
                    parseWhiteSpace input (index, line, col) (SymbolTable symbolTable) parsedSubexpressions
                else 
                    match makeToken currWord with
                    | ErrSome token -> parseWhiteSpace input.[1..] (index+1, line, col+1) (SymbolTable symbolTable) (parsedSubexpressions@[Token token])
                    | Error msg -> (Error msg, (index, line, col))
            | _ ->
                // Other token (either keyword or identifier)
                parser input.[1..] (index+1, line, col+1) (SymbolTable symbolTable) parsedSubexpressions (currWord + (string input.[0]))

    let parse input symbolTable =
        if input = "" then
            (Error "Unexpected end of file", (0,0,0))
        else if input.[0] = '(' then
            parser input.[1..] (0,0,0) symbolTable [] ""
        else
            (Error "Expected start of expression at beginning of file", (0,0,0))
