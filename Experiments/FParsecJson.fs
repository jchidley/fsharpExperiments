#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\xunit.runner.visualstudio\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution\lib\net452\xunit.execution.desktop.dll"
    #r @"..\packages\FParsec\lib\net40-client\FParsecCS.dll"
    #r @"..\packages\FParsec\lib\net40-client\FParsec.dll"
    #r @"..\packages\FsCheck\lib\net452\FsCheck.dll"
    #r @"..\packages\FsCheck.Xunit\lib\net452\FsCheck.Xunit.dll"
#endif

namespace fSharpExperiments

module JSONParser = 
    open FParsec

    type UserState = unit // doesn't have to be unit, of course
    type Parser<'t> = Parser<'t, UserState>

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
    exception ParseError of string
    let parse parser input = 
        match run parser input with 
            | Success (result, _, _) -> result
            | Failure (error, _, _) -> raise (ParseError error) 

    type Json = 
        | JNull
        | JBool of bool
        | JNumber of float 
        | JArray of Json list
        | JObject of Map<string, Json>

    let pJNull:Parser<_> = stringReturn "null" JNull

    let pJBool:Parser<_> = stringReturn "true" (JBool true) <|> stringReturn "false" (JBool false)

    let pJNumber:Parser<_> = pfloat |>> JNumber

module JSONParserTest = 
    open JSONParser
    open Xunit
    open FsCheck.Xunit

    let stringFloatThatCanBeRoundTripped (x:float) =
        let y = 
            match x with
            | float.PositiveInfinity -> System.Double.MaxValue
            | float.NegativeInfinity -> System.Double.MinValue
            | _ -> x
        y.ToString("G17")

    [<Fact;Trait("Parse","Json")>]
    let ``create JSON Objects`` () =
        let jn = JNull
        let jbt = JBool true
        let jbf = JBool false
        let jl = [jn;jbt;jbf]
        let jo = JObject (Map.empty.Add("null",JNull))
        let actual = (jn, jbt, jbf, jl, jo)
        let expected = (JNull, JBool true, JBool false, [JNull; JBool true; JBool false], JObject(Map[("null",JNull)]))
        Assert.Equal(expected, actual)
    
    [<Fact;Trait("Parse","Json")>]
    let ``parse JSON null`` () =
        let expected = JNull
        let actual = JSONParser.parse pJNull "null"
        Assert.Equal(expected, actual)

    [<Property;Trait("Parse","Json")>]
    let ``parse JSON numbers`` (x:float) =
        let y = stringFloatThatCanBeRoundTripped x
        let expected = System.Double.Parse(y)
        let (JNumber actual) = JSONParser.parse pJNumber y
        Assert.Equal(expected, actual)
    
    [<Fact;Trait("Parse","Json")>]
    let ``parse JBool`` () =
        let expected = JBool true, JBool false
        let actual = JSONParser.parse pJBool "true", JSONParser.parse pJBool "false"
        Assert.Equal(expected, actual)