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
                | ' ' | '\t' ->
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
            | _ -> AstNode(Value(ValueToken(Const(Int(number)))))

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
        let parseComment input (index, line, col) parserState = 
            let rec loop input (index, line, col) parserState = 
                if input = "" then
                    parser input (index, line, col) parserState
                else
                    match input.[0] with
                    | '\n' ->
                        parser input (index, line, col) parserState
                    | _ ->
                        loop input.[1..] (index+1,line,col+1) parserState
            if input = "" then
                parser input (index, line, col) parserState
            else if input.[..1] = "//" then
                loop input.[1..] (index+1, line, col+1) parserState
            else
                // Not a comment, only a single /. Return to parser with the / added to the word buffer.
                let (symbolTable, parsedSubexpressions, currWord) = parserState
                parser input.[1..] (index+1, line, col+1)
                    (ParserState(
                        symbolTable,
                        parsedSubexpressions,
                        (currWord + (string '/'))
                    ))

        // Parse string
        let rec parseString input (index, line, col) currString parserState = 
            if input = "" then
                (Error("Unexpected end of file"), (index, line, col))
            else
                match input.[0] with
                | '"' ->
                    let (symbolTable, parsedSubexpressions, _) = parserState
                    parser input.[1..] (index+1, line, col+1)
                        (ParserState(
                            symbolTable,
                            (parsedSubexpressions@[
                                AstNode(Value(ValueToken(Const(Str(currString)))))
                            ]),""
                        ))
                | '\n' -> parseString input.[1..] (index+1, line+1, col) (currString + (string '\n')) parserState
                | '\\' ->
                    if input.[1..] = "" then
                        (Error("Unexpected end of file"), (index, line, col))
                    else
                        parseString input.[2..] (index+2, line, col+2) 
                            (currString + (
                                match input.[1] with
                                | '"' -> "\""
                                | '\\' -> "\\"
                                | 'n' -> "\n"
                                | 't' -> "\t"
                                | ch -> "\\" + (string ch)
                            )
                        ) parserState
                | ch -> parseString input.[1..] (index+1, line, col+1) (currString + (string ch)) parserState

        // Turn string into token
        let makeToken word : Token = 
            match word with
            | "true" -> ValueToken(Const(Bool true))
            | "false" -> ValueToken(Const(Bool false))
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
            | _ ->
                match currArgs.[0] with
                | Value(ValueToken(tkn)) when currArgs.Length > 1 ->
                    Error("Attempt to use value as function: " + tkn.ToString())
                | Value(ValueToken(_)) as tkn ->
                    ErrSome(tkn)
                | _ ->
                    ErrSome(Node(currArgs.[0], currArgs.[1..]))

        //printfn "---------------------------\n%s" input
        //printfn "line:%d, col:%d, index:%d" line col index
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
                | (Error msg, pos) -> (Error msg, pos)
                | (ErrSome (astNode, newSymbolTable), (newIndex, newLine, newCol)) ->
                    if (1+newIndex-index) > input.Length then
                        (Error "Unexpected end of file", (index, line, col))
                    else
                        parser input.[(1+newIndex-index)..] (newIndex+1, newLine, newCol+1)
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
            | '/' ->
                // Comment (or just a /, let parseComment decide.)
                parseComment input (index, line, col) currState
            | ' ' | '\t' | '\n' ->
                // End of word
                let (newIndex, newLine, newCol) =
                    if input.[0] = '\n' then
                        (index+1, line+1, col)
                    else
                        (index+1, line, col+1)
                if currWord = "" then
                    parseWhiteSpace input (index, line, col) currState
                else
                    parseWhiteSpace input.[1..] (newIndex, newLine, newCol+1)
                        (ParserState(
                            (SymbolTable symbolTable),
                            (parsedSubexpressions@[Token(makeToken currWord)]),
                            ""
                        ))
            | '"' ->
                // String
                if currWord = "" then
                    parseString input.[1..] (index+1, line, col+1) "" currState
                else
                    (Error("Unexpected \" in middle of token"), (index+1, line, col+1))
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
            (ErrSome(Empty, symbolTable), (0,0,0))
        else if input.[0] = '(' then
            match parser input.[1..] (0,0,0) (ParserState(symbolTable, [], "")) with
            | (Error(_),_) as err -> err
            | (ErrSome(_), (index, line, col)) as result ->
                if (input.Length - 2) > (index + 1) then
                    warn "Ignored trailing text after expression" (index, line, col)
                result
        else
            (Error "Expected start of expression at beginning of file", (0,0,0))
