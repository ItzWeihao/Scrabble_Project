// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open System
    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many (pchar ' ' <|> pchar '\n') <?> "spaces"
    let spaces1        = many1 (pchar ' ') <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let braces p = pchar '{' >*>. p .>*> pchar '}'

    let pid = (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_') |>> (fun (x, y) -> (x::y) |> String.Concat)
    
    let unop op p1 = op >*>. p1
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CtomParse, cref = createParserForwardedToRef<cExp>()
    let BtomParse, bref = createParserForwardedToRef<bExp>()
    let BconParse, conref = createParserForwardedToRef<bExp>()
    let BequParse, eref = createParserForwardedToRef<bExp>()
    let StmParse, sref = createParserForwardedToRef<stm>()
    let SseqParse, seqref = createParserForwardedToRef<stm>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (N -1, x)) |>> Mul <?> "Neg"
    let VParse = pid |>> V <?> "Variable"
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let CharToInt = unop pCharToInt CtomParse |>> CharToInt <?> "CharToInt"
    
    do aref := choice [CharToInt; NegParse; NParse; PVParse; VParse; ParParse]
    let AexpParse = TermParse 

    let CParParse = parenthesise CtomParse
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let CVParse = unop pCharValue AtomParse |>> CV <?> "CV"
    let ToUpperParse = unop pToUpper CtomParse |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop pToLower CtomParse |>> ToLower <?> "ToLower"
    let IntToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "IntToChar"
    do cref := choice [CParParse; CParse; CVParse; ToUpperParse; ToLowerParse; IntToCharParse]
    let CexpParse = CtomParse

    let Bequals = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "AEq"
    let BnotEquals = binop (pstring "<>") AexpParse AexpParse |>> (fun (x,y) -> x.<>.y)
    let BLess = binop (pchar '<') AexpParse AexpParse |>> ALt
    let BLessEqual = binop (pstring "<=") AexpParse AexpParse |>> (fun (x,y) -> x.<=.y)
    let BMore = binop (pchar '>') AexpParse AexpParse |>> (fun (x,y) -> x.>.y)
    let BMoreEqual = binop (pstring ">=") AexpParse AexpParse |>> (fun (x,y) -> x.>=.y)
    do eref := choice [Bequals; BnotEquals; BLess; BLessEqual; BMore; BMoreEqual; BtomParse]
    
    let BParParse = parenthesise BconParse
    let BTrue = pTrue |>> (fun _ -> TT) <?> "TT"
    let BFalse = pFalse |>> (fun _ -> FF) <?> "FF"
    let BNot = unop (pchar '~') BconParse |>> Not
    do bref := choice [BTrue; BFalse; BNot; BParParse]
    
    let BexpParse = BconParse

    let Sseq = binop (pchar ';') StmParse SseqParse |>> Seq <?> "Seq"
    do seqref := choice [Sseq; StmParse]
    
    let SBrace = braces SseqParse
    let Sass = binop (pstring ":=") pid AexpParse |>> Ass
    let Sdeclair = pdeclare >>. whitespaceChar >>. pid |>> Declare <?> "Declare"
    let Site = pif >*>. BParParse .>*> pthen .>*>. SBrace .>*> pelse .>*>. SBrace |>> (fun ((b,stm1),stm2) -> ITE (b, stm1, stm2))
    let Sit = pif >*>. BParParse .>*> pthen .>*>. SBrace |>> (fun (b, stm1) -> ITE (b, stm1, Skip))
    let Swhile = pwhile >*>. BParParse .>*> pdo .>*>. SBrace |>> While
    do sref := choice [SBrace; Sass; Sdeclair; Site; Sit; Swhile]
    let stmntParse = SseqParse

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    (*
    let parseSquareProg sqp : square = sqp |> Map.map (fun _ v -> run stmntParse v |> getSuccess |> stmntToSquareFun)
    let parseBoardProg = run stmntParse >> getSuccess >> stmntToBoardFun
    let mkBoard (bp : boardProg) =
        {
            center = bp.center
            defaultSquare =  Map.find bp.usedSquare bp.squares |> parseSquareProg
            squares =
                let m' = Map.map (fun _ -> parseSquareProg) bp.squares
                parseBoardProg bp.prog m'
        }
    *)
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
