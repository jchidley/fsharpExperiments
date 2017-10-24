#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\xunit.runner.visualstudio.2.3.0\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert.2.3.0\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core.2.3.0\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution.2.3.0\lib\net452\xunit.execution.desktop.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsecCS.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsec.dll"
#endif

namespace fSharpExperiments

module FParsecStringTest =
    open FParsec
    open Xunit
    open fSharpExperiments.FParsecFloatTest

    [<Fact;Trait("Parse","String")>]
    let ``parse (many (str "a" <|> str "b")) "abba"`` () =
        let r = parse (many (str "a" <|> str "b")) "abba"
        Assert.True(["a"; "b"; "b"; "a"] = r)

    [<Fact;Trait("Parse","String")>]
    let ``parse (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"`` () =
        let r = parse (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"
        Assert.True(1.0 = r)


    // there's a built in version of "identifier" that works slightly differently and seems more complicated
    let identifier:Parser<string,unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        .>> spaces // skips trailing whitespace

    let r = parse identifier "_"

    [<Fact;Trait("Parse","String")>]
    let ``parse identifier "_test1="`` () =
        let r = parse identifier "_test1="
        Assert.True("_test1" = r)
    
    [<Fact;Trait("Parse","String")>]
    let ``parse identifier "1" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse identifier "1" |> ignore)
        Assert.IsType<ParseError>(ex)
    
    let stringLiteral:Parser<string,unit> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar))

    [<Fact;Trait("Parse","String")>]
    let ``stringLiteral "\"abc\""`` () =
        let r = parse stringLiteral "\"abc\""
        Assert.True("abc" = r)

    let stringTestData = [ [| "\"abc\"";"abc"|];
                            [|"\"abc\\\"def\\\\ghi\"";"""abc"def\ghi"""|]
                            ]
// List of objects?
//                             |> Seq.map (fun (a,b) -> [|a; b|])

// http://fortysix-and-two.blogspot.co.uk/2010/12/f-xunit-theories-and-inlinedata.html
// and https://stackoverflow.com/questions/35026735/in-f-how-do-you-pass-a-collection-to-xunits-inlinedata-attribute/35127997#35127997

    //[<Theory>]
    //[<InlineData("\"abc\"","abc")>]
    //[<InlineData("\"abc\\\"def\\\\ghi\"","""abc"def\ghi""")>]
    //[<InlineData("\"abc\\def\"","""abc\def""")>]
    [<Theory; MemberData("stringTestData")>]
    [<Trait("Parse","String")>]
    let ``stringLiteral PASS``(toParse:string, expected:string) =
        let actual = parse stringLiteral toParse
        Assert.Equal(expected, actual)

    [<Fact;Trait("Parse","String")>]
    let ``stringLiteral FAIL`` () = 
        let ex = Record.Exception(fun () -> parse stringLiteral "\"abc\\def\"" |> ignore)
        Assert.IsType<ParseError>(ex)
    

    let stringLiteral2:Parser<string,unit> =
        let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                                | 'n' -> "\n"
                                                                | 'r' -> "\r"
                                                                | 't' -> "\t"
                                                                | c   -> string c)
        between (pstring "\"") (pstring "\"")
                (manyStrings (normalCharSnippet <|> escapedChar))

    [<Theory; MemberData("stringTestData")>]
    [<Trait("Parse","String")>]
    let ``stringLiteral2 PASS``(toParse:string, expected:string) =
        let actual = parse stringLiteral toParse
        Assert.Equal(expected, actual)

    [<Fact;Trait("Parse","String")>]
    let ``stringLiteral2 FAIL`` () = 
        let ex = Record.Exception(fun () -> parse stringLiteral "\"abc\\def\"" |> ignore)
        Assert.IsType<ParseError>(ex)
    
    let stringLiteral3:Parser<string,unit> =
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                                    | 'n' -> "\n"
                                                                    | 'r' -> "\r"
                                                                    | 't' -> "\t"
                                                                    | c   -> string c)
        between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedChar)

    [<Theory; MemberData("stringTestData")>]
    [<Trait("Parse","String")>]
    let ``stringLiteral3 PASS``(toParse:string, expected:string) =
        let actual = parse stringLiteral toParse
        Assert.Equal(expected, actual)

    [<Fact;Trait("Parse","String")>]
    let ``stringLiteral3 FAIL`` () = 
        let ex = Record.Exception(fun () -> parse stringLiteral "\"abc\\def\"" |> ignore)
        Assert.IsType<ParseError>(ex)

module FParsecTogetherTest =
    open FParsec
    open FParsecFloatTest
    open Xunit
    open FParsecStringTest

    let product = pipe2 float_ws (str_ws "*" >>. float_ws)
                    (fun x y -> x * y)
    
    [<Fact;Trait("Parse","Together")>]
    let ``product 3 * 5``() =
        let actual = parse product "3 * 5"
        Assert.Equal(15.0, actual)

    type StringConstant = StringConstant of string * string

    let stringConstant = pipe3 identifier (str_ws "=") stringLiteral
                               (fun id _ str -> StringConstant(id, str))

    [<Fact;Trait("Parse","Together")>]
    let ``stringConstant "myString = \"stringValue\""``() =
        let actual = parse stringConstant "myString = \"stringValue\""
        let expected = StringConstant ("myString","stringValue")
        Assert.Equal(expected, actual)

    [<Fact;Trait("Parse","Together")>]
    let ``(float_ws .>>. (str_ws "," >>. float_ws)) "123, 456"``() =
        let actual = parse (float_ws .>>. (str_ws "," >>. float_ws)) "123, 456"
        let expected = (123.0, 456.0)
        Assert.Equal(expected, actual)
    
    let boolean = (stringReturn "true"  true) <|> (stringReturn "false" false)

    //let booleanTestData = [ [| "false"; false|];
    //                        [|"true";true|]
    //                        ]

    [<Fact;Trait("Parse","Together")>]
    let ``boolean "true"``() =
        let actual = parse boolean "true"
        let expected = true
        Assert.Equal(expected, actual)
    
    [<Fact;Trait("Parse","Together")>]
    let ``boolean "false"``() =
        let actual = parse boolean "false"
        let expected = false
        Assert.Equal(expected, actual)

    [<Fact;Trait("Parse","Together")>]
    let ``((ws >>. str "a") <|> (ws >>. str "b")) " b" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse ((ws >>. str "a") <|> (ws >>. str "b")) " b" |> ignore)
        Assert.IsType<ParseError>(ex)

    [<Fact;Trait("Parse","Together")>]
    let ``(ws >>. (str "a" <|> str "b")) " b"``() =
        let actual = parse (ws >>. (str "a" <|> str "b")) " b"
        let expected = "b"
        Assert.Equal(expected, actual)
        
