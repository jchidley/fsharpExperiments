#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\xunit.runner.visualstudio.2.3.0\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert.2.3.0\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core.2.3.0\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution.2.3.0\lib\net452\xunit.execution.desktop.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsecCS.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsec.dll"
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
    #r @"..\packages\FsCheck.Xunit.2.10.3\lib\net452\FsCheck.Xunit.dll"
#endif

namespace fSharpExperiments

module JSONParser = 
    open FParsec
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open System.IO.Compression

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

    let pJNull:Parser<_> = stringReturn "null" JNull
    
    [<Fact;Trait("Parse","Json")>]
    let ``parse JSON null`` () =
        let expected = JNull
        let actual = parse pJNull "null"
        Assert.Equal(expected, actual)

    let pJBool:Parser<_> = stringReturn "true" (JBool true) <|> stringReturn "false" (JBool false)

    [<Fact;Trait("Parse","Json")>]
    let ``parse JSON booleans`` () =
        let expected = JBool true, JBool false
        let actual = parse pJBool "true", parse pJBool "false"
        Assert.Equal(expected, actual)
    
    let pJNumber:Parser<_> = pfloat |>> JNumber

    // https://github.com/fscheck/FsCheck/issues/103
    type Letters =
        static member Char() = Gen.elements ['A' .. 'Z'] |> Arb.fromGen

    [<Property( Arbitrary=[| typeof<Letters> |])>]
    let ``Diamond is non-empty`` (letter : char) =
        let actual = letter
        let chars = ['A'..'Z']
        Assert.True(chars |> List.contains letter)

    [<Property;Trait("Parse","Json")>]
    let ``parse JSON numbers`` (x:float) =
        let y = if (x = infinity || x = -infinity) then 0.0 else x
        let expected = JNumber (System.Double.Parse(y.ToString("G17")))
        let actual = parse pJNumber (y.ToString("G17"))
        Assert.Equal(expected, actual)
    
    [<Fact;Trait("Parse","Json")>]
    let ``parse JBool`` () =
        let expected = JBool true, JBool false
        let actual = parse pJBool "true", parse pJBool "false"
        Assert.Equal(expected, actual)

