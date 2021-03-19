namespace FsLisp
open Types
open System

module Parser = 
    // Parts that will be parsed, and assembled into an AST
    type ExpressionPart =
    | AstNode of AstNode    // Subexpression
    | Token of Token        // Indentifier or Constant

    type ParserPosition = (int * int * int) // input index * line nr * column nr

    type ParserState = (
            SymbolTable *
            ExpressionPart list * // parsed subexpressions
            string  // current word
        )

    let rec parser
        input (index, line, col)
        ((
            (SymbolTable symbolTable),
            parsedSubexpressions,
            currWord
        ) as currState) : (Error<AstNode * SymbolTable> * ParserPosition) =

        // Skip over areas of whitespace
        let rec parseWhiteSpace input (index, line, col) parserState =
            if input = "" then
                parser input (index, line, col) parserState
            else
                match input.[0] with
                | '\n' -> parseWhiteSpace input.[1..] (index+1, line+1, 0) parserState
                | ' ' | '\n' | '\t' ->
                    parseWhiteSpace input.[1..] (index+1, line, col+1) parserState
                | _ -> parser input (index, line, col) parserState

        // Build a number from an inverted list of number-characters
        let rec buildNumber number numberBuf pos =
            match numberBuf with
            | head :: tail ->
                buildNumber (
                    number + (
                        int(Char.GetNumericValue(head))
                    ) * (pown 10 pos)
                ) tail (pos + 1)
            | _ -> AstNode(Value(Const(Int(number))))

        // Parse a number and add it to the list of parsedSubexpressions, which is then passed on for parsing the rest of the expression
        let rec parseNumber input (index, line, col) numberBuf parserState =
            if input = "" then
                let (symbolTable, parsedSubexpressions, _) = parserState
                parser input (index, line, col)
                    (ParserState(
                        symbolTable,
                        (parsedSubexpressions@[
                            (buildNumber 0 numberBuf 0)
                        ]),""
                    ))
            else 
                match input.[0] with
                | _ when Char.IsNumber(Convert.ToChar(input.[0])) ->
                     parseNumber
                        input.[1..] (index+1, line, col+1) 
                         (input.[0] :: numberBuf) parserState
                | ' ' | '\t' | '\n' | ')' ->
                    let (symbolTable, parsedSubexpressions, _) = parserState
                    parser input (index, line, col)
                        (ParserState(
                            symbolTable,
                            (parsedSubexpressions@[
                                (buildNumber 0 numberBuf 0)
                            ]),""
                        ))
                | _ ->
                    (Error("Unexpected character " + (string input.[0]) + " in number"),(index, line, col))

        // Parse comment
        let rec parseComment input (index, line, col) parserState = 
            if input = "" then
                parser input (index, line, col) parserState
            else
                match input.[0] with
                | '\n' ->
                    parser input (index, line, col) parserState
                | _ ->
                    parseComment input.[1..] (index+1,line,col+1) parserState

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
                ErrSome(Empty)
            | _ when currArgs.Length = 1 ->
                ErrSome(currArgs.[0])
            | _ ->
                ErrSome(Node(currArgs.[0], currArgs.[1..]))

        if input = "" then
            (Error "Unexpected end of file", (index, line, col))
        else
            match input.[0] with
            | _ when Char.IsNumber(Convert.ToChar(input.[0])) && currWord = "" ->
                // ^- && currWord = "" allows for other tokens to contain numerals, so long as they do not begin with them
                // Numbers
                parseNumber input (index, line, col) [] currState
            | '(' ->
                // Subexpression
                match parser input.[1..] (index+1, line, col+1) (ParserState((SymbolTable symbolTable), [], "")) with
                | (Error msg, (newIndex, newLine, newCol)) -> (Error msg, (newIndex, line, col))
                | (ErrSome (astNode, newSymbolTable), (newIndex, newLine, newCol)) ->
                    if (1+newIndex-index) > input.Length then
                        (Error "Unexpected end of file", (index, line, col))
                    else
                        parser input.[(1+newIndex-index)..] (newIndex+1, newLine, newCol)
                            (ParserState(
                                newSymbolTable,
                                (
                                    if currWord = "" then
                                        parsedSubexpressions@[AstNode astNode]
                                    else
                                        (parsedSubexpressions@[Token (makeToken currWord)])@[AstNode astNode]
                                ),""
                            ))
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
            | ' ' | '\t' | '\n' ->
                // End of word
                if currWord = "" then
                    parseWhiteSpace input (index, line, col) currState
                else if currWord.Length >= 2 && currWord.[..1] = "//" then
                    //Comment
                    parseComment input (index, line, col) (ParserState((SymbolTable symbolTable), parsedSubexpressions, ""))
                                                            // TODO: ^- this is cursed, can't replace with currState...
                else
                    parseWhiteSpace input.[1..] (index+1, line, col+1)
                        (ParserState(
                            (SymbolTable symbolTable),
                            (parsedSubexpressions@[Token(makeToken currWord)]),
                            ""
                        ))
            | _ ->
                // Other token (either keyword or identifier)
                parser input.[1..] (index+1, line, col+1) 
                    (ParserState(
                        (SymbolTable symbolTable),
                        parsedSubexpressions,
                        (currWord + (string input.[0]))
                    )) 

    let parse input symbolTable =
        if input = "" then
            (Error "Unexpected end of file", (0,0,0))
        else if input.[0] = '(' then
            parser input.[1..] (0,0,0) (ParserState(symbolTable, [], ""))
        else
            (Error "Expected start of expression at beginning of file", (0,0,0))
