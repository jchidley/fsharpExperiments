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

module FParsecFloatTest =
    open FParsec
    open Xunit

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
    exception ParseError of string
    let parse parser input = 
        match run parser input with 
            | Success (result, _, _) -> result
            | Failure (error, _, _) -> raise (ParseError error) 

    [<Fact;Trait("Parse","Float")>]
    let ``"1.25" is parsed as 1.25f`` () =
        let r = parse pfloat "1.25"
        Assert.Equal(1.25, r)

    [<Fact;Trait("Parse","Float")>]
    let ``pfloat "1.25E 3" fails`` () = 
        // https://www.richard-banks.org/2015/07/stop-using-assertthrows-in-your-bdd.html
        // pfloat should return a float which needs to be ignored
        let ex = Record.Exception(fun () -> parse pfloat "1.25E 3" |> ignore)
        Assert.IsType<ParseError>(ex)
        // or http://www.bjoernrochel.de/2010/04/19/testing-f-code-with-xunit-net-on-net-4-0/
        Assert.Throws<ParseError>(fun () -> parse pfloat "1.25E 3" |> ignore)
    
//    type UserState = unit // doesn't have to be unit, of course
//    type Parser<'t> = Parser<'t, UserState>

    let str s = pstring s
    let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"
// need this line if you don't run an example first.
//     let floatBetweenBrackets:Parser<_> = str "[" >>. pfloat .>> str "]"
// the above line not needed if you do this:    
    test floatBetweenBrackets "[1.0]" |> ignore
    
    [<Fact;Trait("Parse","Float")>]
    let ``Parsing [1.0] works`` () =
        let r = parse floatBetweenBrackets "[1.0]"
        Assert.Equal(1.0,r)
    
    [<Fact;Trait("Parse","Float")>]
    let ``parse floatBetweenBrackets "[]" fails`` () = 
        let ex = Record.Exception(fun () -> parse floatBetweenBrackets "[]"  |> ignore)
        Assert.IsType<ParseError>(ex)

    [<Fact;Trait("Parse","Float")>]
    let ``parse floatBetweenBrackets "[1.0" fails`` () = 
        let ex = Record.Exception(fun () -> parse floatBetweenBrackets "[1.0"  |> ignore)
        Assert.IsType<ParseError>(ex)

    let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

    let floatBetweenBrackets2 = pfloat |> betweenStrings "[" "]"
    test floatBetweenBrackets2 "[2.0]" |> ignore
    let floatBetweenDoubleBrackets = pfloat |> betweenStrings "[[" "]]"
    test floatBetweenDoubleBrackets "[[2.0]]" |> ignore

    [<Fact;Trait("Parse","Float")>]
    let ``parse floatBetweenBrackets2 "[2.0]"`` () = 
        let r = parse floatBetweenBrackets "[2.0]"
        Assert.Equal(2.0,r)

    [<Fact;Trait("Parse","Float")>]
    let ``parse floatBetweenDoubleBrackets "[[3.0]]"`` () = 
        let r = parse floatBetweenDoubleBrackets "[[3.0]]"
        Assert.Equal(3.0,r)

// "between" is in the FParsec library
// let between pBegin pEnd p  = pBegin >>. p .>> pEnd
    let betweenStrings3 s1 s2 p = p |> between (str s1) (str s2)
    let betweenCurlies = pfloat |> betweenStrings3 "{" "}"
    test betweenCurlies "{4.0}" |> ignore

    [<Fact;Trait("Parse","Float")>]
    let ``parse betweenCurlies "{4.0}"`` () = 
        let r = parse betweenCurlies "{4.0}"
        Assert.Equal(4.0,r)
    
    let r = parse (many floatBetweenBrackets) "[2][3][4]"

    [<Fact;Trait("Parse","Float")>]
    let ``parse (many floatBetweenBrackets) ""`` () = 
        let r = parse (many floatBetweenBrackets) ""
        Assert.True(([]:float list) = r)

    [<Fact;Trait("Parse","Float")>]
    let ``parse (many floatBetweenBrackets) "[1.0]"`` () = 
        let r = parse (many floatBetweenBrackets) "[1.0]"
        Assert.True([1.0] = r)

    [<Fact;Trait("Parse","Float")>]
    let ``parse (many floatBetweenBrackets) ""[2][3][4]""`` () = 
        let r = parse (many floatBetweenBrackets) "[2][3][4]"
        Assert.True([2.0;3.0;4.0] = r)

    [<Fact;Trait("Parse","Float")>]
    let ``parse (many floatBetweenBrackets) "[1][2.0E]" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse (many floatBetweenBrackets) "[1][2.0E]"  |> ignore)
        Assert.IsType<ParseError>(ex)
    
    [<Fact;Trait("Parse","Float")>]
    let ``parse (many1 floatBetweenBrackets) "(1)" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse (many1 floatBetweenBrackets) "(1)" |> ignore)
        Assert.IsType<ParseError>(ex)
    
    test (many1 (floatBetweenBrackets <?> "float between brackets")) "(1)"

    let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"
    parse floatList "[4,5,6]" |> ignore

    [<Fact;Trait("Parse","Float")>]
    let ``parse floatList "[1,2,3]"`` () = 
        let r = parse floatList "[1,2,3]"
        Assert.True([1.0; 2.0; 3.0] = r)

    [<Fact;Trait("Parse","Float")>]
    let ``parse floatList "[1.0,]" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse floatList "[1.0,]" |> ignore)
        Assert.IsType<ParseError>(ex)
    
    [<Fact;Trait("Parse","Float")>]
    let ``parse floatList "[1.0,2.0" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse floatList "[1.0,2.0" |> ignore)
        Assert.IsType<ParseError>(ex)

    [<Fact;Trait("Parse","Float")>]
    let ``parse floatBetweenBrackets "[1.0, 2.0]" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse floatBetweenBrackets "[1.0, 2.0]" |> ignore)
        Assert.IsType<ParseError>(ex)
    
    let ws = spaces
    let str_ws s = pstring s .>> ws
    let float_ws = pfloat .>> ws
    let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"
    parse numberList @"[ 1 ,
                          2 ] " |> ignore
    test numberList @"[ 1,
                         2; 3]"

    let numberListFile = ws >>. numberList .>> eof
    test numberListFile " [1, 2, 3] [4]"
