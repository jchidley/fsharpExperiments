#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
    #r @"..\packages\xunit.runner.visualstudio.2.3.0\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert.2.3.0\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core.2.3.0\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution.2.3.0\lib\net452\xunit.execution.desktop.dll"
    #r @"..\packages\FsCheck.Xunit.2.10.3\lib\net452\FsCheck.Xunit.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsecCS.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsec.dll"
#endif

namespace fSharpExperiments

module FParsecStringTest =
    open FParsec
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open fSharpExperiments.FParsecFloatTest

    [<Fact>]
    let ``parse (many (str "a" <|> str "b")) "abba"`` () =
        let r = parse (many (str "a" <|> str "b")) "abba"
        Assert.True(["a"; "b"; "b"; "a"] = r)

    [<Fact>]
    let ``parse (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"`` () =
        let r = parse (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"
        Assert.True(1.0 = r)


    // there's a built in version of "identifier" that works slightly differently and seems more complicated
    let identifier =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        .>> ws // skips trailing whitespace

    let r = parse identifier "_"

    [<Fact>]
    let ``parse identifier "_test1="`` () =
        let r = parse identifier "_test1="
        Assert.True("_test1" = r)
    
    [<Fact>]
    let ``parse identifier "1" FAIL`` () = 
        let ex = Record.Exception(fun () -> parse identifier "1" |> ignore)
        Assert.IsType<ParseError>(ex)
    

