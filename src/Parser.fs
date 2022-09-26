module Parser

open FParsec
open System

let curry f = fun x y -> f (x, y)
let stringLiteral =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar) .>> spaces

let constLiteral str result = pstring str .>> spaces |>> (fun _ -> result)
let operator (p1: Parser<'val1, unit>) (p2: Parser<'val2, unit>) str result = pipe3 p1 (pstring str .>> spaces) p2 (fun a b c -> result a c)
let intParser: Parser<int, unit> = numberLiteral NumberLiteralOptions.AllowMinusSign "integer" .>> spaces |>> (fun num -> int num.String)
let nameParser: Parser<string, unit> =
    let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'

    identifier (IdentifierOptions(
                    isAsciiIdStart = isAsciiIdStart,
                    isAsciiIdContinue = isAsciiIdContinue,
                    normalization = System.Text.NormalizationForm.FormKC,
                    normalizeBeforeValidation = true,
                    allowAllNonAsciiCharsInPreCheck = true)) .>> spaces

let openBrace: Parser<char, unit> = pchar '(' .>> spaces
let closeBrace: Parser<char, unit> = pchar ')' .>> spaces
let openSquareBrace: Parser<char, unit> = pchar '[' .>> spaces
let closeSquareBrace: Parser<char, unit> = pchar ']' .>> spaces
let openCurlyBrace: Parser<char, unit> = pchar '{' .>> spaces
let closeCurlyBrace: Parser<char, unit> = pchar '}' .>> spaces

type Const =
    | Universe
    | Refl
    | StrType
    | Str of String
    | IntType
    | Int of int
    | FloatType
    | Float of float


let universeLiteral = constLiteral "*" Universe
let reflLiteral = constLiteral "refl" Refl
let strTypeLiteral = constLiteral "String" StrType
let intTypeLiteral = constLiteral "Int" IntType
let floatTypeLiteral = constLiteral "Float" FloatType

let intLiteral = numberLiteral NumberLiteralOptions.AllowMinusSign "integer" .>> spaces |>> (fun num -> Int (int32 num.String))
let floatLiteral = numberLiteral (NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction) "float" |>> (fun num -> Float (float num.String))

let parserList = [
    universeLiteral
    reflLiteral
    strTypeLiteral
    intTypeLiteral
    floatTypeLiteral
    stringLiteral |>> Str
    intLiteral
    floatLiteral
]

let constParser: Parser<Const, unit> = choice parserList

// special functions: proj, match
type AST =
    | TypeAnn of AST * AST // AST : AST // EXPR6
    | Pi of AST * AST // AST -> AST // EXPR5
    | CoProd of AST * AST // AST | AST // EXPR4
    | Eq of AST * AST // AST ~ AST // EXPR3
    | Si of  AST * AST  // AST, AST // EXPR2
    | CoProdIn of AST * AST // AST <|> AST // EXPR1
    | Fun of string * AST * AST // { Name @ AST = AST } // EXPR0
    | Lam of  AST * AST // { AST = AST }
    | App of list<AST> // (AST AST ...)
    | Pair of list<AST> // [AST AST AST AST]
    | Constant of Const // const
    | Variable of string // name


let astParser, astParserRef = createParserForwardedToRef<AST, unit>()

let innerFunParser = (pipe5 nameParser (pchar '@' .>> spaces) astParser (pchar '=' .>> spaces) astParser (fun a _ b _ c -> Fun (a, b, c)))
let innerLambdaParser = (pipe3 astParser (pchar '=' .>> spaces) astParser (fun a  _ b -> Lam (a, b)))

let funParser = between openCurlyBrace closeCurlyBrace innerFunParser
let lambdaParser = between openCurlyBrace closeCurlyBrace innerLambdaParser
let appParser = between openBrace closeBrace (many astParser) |>> (fun list -> App list)
let pairParser = between openSquareBrace closeSquareBrace (many astParser) |>> (fun list -> Pair list)

let expr0Parser: Parser<AST, unit> = choice [
    constParser |>> Constant;
    nameParser |>> Variable;
    funParser;
    lambdaParser;
    appParser;
    pairParser;
] 

let app f ast = function
    | None -> ast
    | Some ast' -> (curry f) ast ast'

let expr1Parser = pipe2 expr0Parser (opt (skipString "<|>" >>. spaces >>. expr0Parser)) (app CoProdIn)

let siOp: Parser<_, unit> = skipString "," .>> spaces >>% (curry Si)
let expr2Parser = chainl1 expr1Parser siOp

let expr3Parser = pipe2 expr2Parser (opt (skipString "~" >>. spaces >>. expr2Parser))  (app Eq)

let expr4Parser = pipe2 expr3Parser (opt (skipString "|" >>. spaces >>. expr3Parser))  (app CoProd)

let expr5Parser = pipe2 expr4Parser (opt (skipString ":" >>. spaces >>. expr4Parser))  (app TypeAnn)

let arrowOp: Parser<_, unit> = skipString "->" .>> spaces >>% (curry Pi)
let expr6Parser = chainr1 expr5Parser arrowOp 


astParserRef := expr6Parser
