#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsecCS.dll"
    #r @"..\packages\FParsec.1.0.3\lib\net40-client\FParsec.dll"
#endif

namespace fSharpExperiments

module FsCheckTest =
    // open NUnit.Framework
    // http://fsharpforfunandprofit.com/posts/low-risk-ways-to-use-fsharp-at-work-3/#test-fscheck
    // http://fsharpforfunandprofit.com/posts/property-based-testing/
    // http://fsharpforfunandprofit.com/posts/property-based-testing-2/
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open System
    open NodaTime

    open FParsec

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
    exception ParseError of string
    let parse parser input = 
        match run parser input with 
            | Success (result, _, _) -> result
            | Failure (error, _, _) -> raise (ParseError error) 

    type VersionGenerator =
        static member Version() =
            Arb.generate<byte>
            |> Gen.map int
            |> Gen.four
            |> Gen.map (fun (ma, mi, bu, re) -> Version(ma, mi, bu, re))
            |> Arb.fromGen
    
    Arb.register<VersionGenerator>() |> ignore 

    let intGen = Arb.generate<int>
    let timeGen = Arb.generate<System.DateTime>
    let versionGen = Arb.generate<Version>

    let randInts = intGen 
                    |> Gen.sample 20 1000000 
                    |> Seq.groupBy id 
                    |> Seq.map (fun (k,v) -> (k,Seq.length v))
                    |> Seq.sortBy (fun (k,v) -> k)
                    |> Seq.toList 

    let randVersions = Gen.sample 10 10 versionGen

    let randDates = Gen.choose (1, 30)
                    |> Gen.map (fun i -> DateTime(2019, 11, i, 0,0,0,DateTimeKind.Utc))
                    |> Gen.map (fun i -> Instant.FromDateTimeUtc i)
                    |> Gen.sample 0 1

    [<Property>]
    let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
    [<Property>]
    let revRevIsOrigFail (xs:list<int>) = List.rev xs = xs

    Check.Quick revRevIsOrig
    Check.Quick revRevIsOrigFail

    let revRevIsOrigFloat (xs:list<float>) = List.rev(List.rev xs) = xs
    Check.Quick revRevIsOrigFloat

    [<Fact>]
    let ``When 2 is added to 2 expect 4``() = 
        Assert.Equal(4, 2+2)

    [<Fact>]
    let ``When 2 is added to 2 expect 5``() = 
        Assert.Equal(5, 2+2)
    
    [<Fact>]
    let ``1/0 raises DivideByZeroException`` () =
        Assert.Throws<DivideByZeroException>(fun () -> 1/0 |> ignore)

    type stringFloat = StringFloat of string

    type stringFloatGen =
        static member StringFloat () =
            Arb.generate<float>
            |> Gen.map (fun x -> if (x = infinity || x = -infinity) then 0.0 else x)
            |> Gen.map (fun x -> StringFloat (x.ToString("R")))
            |> Arb.fromGen

    // If using NUnit...
    // [<SetUp>]
    let setup () =
        do Arb.register<stringFloatGen>() |> ignore
     
    [<Property>]
    let ``stringFloat gen``(x:stringFloat) = 
        let (StringFloat str) = x
        let f = System.Double.Parse(str)
        let fp = parse pfloat str
        Assert.Equal(f, fp)
    

(*
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        // NUnitLite if compiled to an exe https://github.com/nunit/docs/wiki/Installation
        // Can also just run NUnit.Console and run against the compiled dll https://github.com/nunit/docs/wiki/Console-Runner
        AutoRun().Execute(argv) // return an integer exit code
*)