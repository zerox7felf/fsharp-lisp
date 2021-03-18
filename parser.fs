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
        input ((index, line, col): ParserPosition) expressionDepth (SymbolTable symbolTable)
        (parsedSubexpressions: ExpressionPart list) currWord : (Error<(AstNode * SymbolTable)> * ParserPosition) =
        // TODO: move parser arguments into a ParserState tuple, so the helper functions can be made shorter...

        // Skip over areas of whitespace
        let rec parseWhiteSpace (input: string) ((index, line, col): ParserPosition) expressionDepth symbolTable parsedSubexpressions =
            if input = "" then
                parser input (index, line, col) expressionDepth symbolTable parsedSubexpressions ""
            else
                match input.[0] with
                | '\n' ->
                    parseWhiteSpace input.[1..] (index+1, line+1, 0) expressionDepth symbolTable parsedSubexpressions
                | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                    parseWhiteSpace input.[1..] (index+1, line, col+1) expressionDepth symbolTable parsedSubexpressions
                | _ ->
                    parser input (index, line, col) expressionDepth symbolTable parsedSubexpressions ""

        // Build a number from an inverted list of number-characters
        let rec buildNumber number numberBuf pos =
            match numberBuf with
            | head :: tail ->
                buildNumber (number + (int(Char.GetNumericValue(head))) * (pown 10 pos)) tail (pos + 1)
            | _ -> number

        // Parse a number and add it to the list of parsedSubexpressions, which is then passed on for parsing the rest of the expression
        // TODO: parse the right way around
        let rec parseNumber (input: string) ((index, line, col): ParserPosition) expressionDepth numberBuf symbolTable parsedSubexpressions =
            if input = "" then
                parser input (index, line, col) expressionDepth symbolTable (
                    parsedSubexpressions@[
                        AstNode(Value(Const(Int(buildNumber 0 numberBuf 0))))
                    ]
                ) ""
            else 
                match input.[0] with
                | _ when Char.IsNumber(Convert.ToChar(input.[0])) ->
                     parseNumber
                         input.[1..] (index+1, line, col+1) expressionDepth
                         (input.[0] :: numberBuf)
                         symbolTable parsedSubexpressions
                | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                    parser input (index, line, col) expressionDepth symbolTable (
                        parsedSubexpressions@[
                            AstNode(Value(Const(Int(buildNumber 0 numberBuf 0))))
                        ]
                    ) ""
                | ')' ->
                    parser input (index, line, col) expressionDepth symbolTable (
                        parsedSubexpressions@[
                            AstNode(Value(Const(Int(buildNumber 0 numberBuf 0))))
                        ]
                    ) ""
                | _ ->
                    (Error ("Unexpected character " + (string input.[0]) + " in number"), (index, line, col))

        // Turn string into token
        let makeToken word : Token = 
            match word with
            | "true" -> Const(Bool true)
            | "false" -> Const(Bool false)
            | _ -> Ident word

        // Turn list of ExpressionParts into AstNode
        let rec expressionListToAst parsedSubexpressions currArgs : Error<AstNode> = 
            match parsedSubexpressions with
            | head :: tail ->
                match head with
                | AstNode node -> 
                    expressionListToAst tail (currArgs@[node])
                | Token token ->
                    expressionListToAst tail (currArgs@[Value token])
            | _ when currArgs.Length = 0 ->
                Error "Empty expression"
            | _ when currArgs.Length = 1 ->
                ErrSome(currArgs.[0])
            | _ ->
                ErrSome(Node(currArgs.[0], currArgs.[1..]))

        printfn "%A" (input.Replace("\n", "\\n"))
        if input = "" then
            (Error "Unexpected end of file", (index, line, col))
        else
            match input.[0] with
            | _ when Char.IsNumber(Convert.ToChar(input.[0])) && currWord = "" ->
                // ^- && currWord = "" allows for other tokens to contain numerals, so long as they do not begin with them
                // Numbers
                parseNumber input (index, line, col) expressionDepth [] (SymbolTable symbolTable) parsedSubexpressions
            | '(' ->
                // Subexpression
                match parser input.[1..] (index+1, line, col+1) (expressionDepth+1) (SymbolTable symbolTable) [] "" with
                | (Error msg, (newIndex, newLine, newCol)) -> (Error msg, (newIndex, line, col))
                | (ErrSome (astNode, newSymbolTable), (newIndex, newLine, newCol)) ->
                    if (1+newIndex-index) > input.Length then
                        (Error "Unexpected end of file", (index, line, col))
                    else
                        parser input.[(1+newIndex-index)..] (newIndex+1, newLine, newCol) expressionDepth newSymbolTable (
                            if currWord = "" then
                                parsedSubexpressions@[AstNode astNode]
                            else
                                (parsedSubexpressions@[Token (makeToken currWord)])@[AstNode astNode]
                        ) ""

            | ')' ->
                // End of expression
                match expressionListToAst
                    (
                        if currWord = "" then
                            parsedSubexpressions
                        else
                            (parsedSubexpressions@[Token(makeToken currWord)])
                    ) [] with
                | ErrSome astNode -> (ErrSome(astNode, SymbolTable symbolTable), (index, line, col))
                | Error msg -> (Error msg, (index, line, col))
            | _ when Char.IsWhiteSpace(Convert.ToChar(input.[0])) ->
                // End of word
                if currWord = "" then
                    parseWhiteSpace input (index, line, col) expressionDepth (SymbolTable symbolTable) parsedSubexpressions
                else 
                    parseWhiteSpace input.[1..] (index+1, line, col+1) expressionDepth (SymbolTable symbolTable) (parsedSubexpressions@[Token(makeToken currWord)])
            | _ ->
                // Other token (either keyword or identifier)
                parser input.[1..] (index+1, line, col+1) expressionDepth (SymbolTable symbolTable) parsedSubexpressions (currWord + (string input.[0]))

    let parse input symbolTable =
        if input = "" then
            (Error "Unexpected end of file", (0,0,0))
        else if input.[0] = '(' then
            parser input.[1..] (0,0,0) 0 symbolTable [] ""
        else
            (Error "Expected start of expression at beginning of file", (0,0,0))
