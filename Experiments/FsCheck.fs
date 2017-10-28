#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
    #r @"..\packages\FsCheck.Xunit.2.10.3\lib\net452\FsCheck.Xunit.dll"
    #r @"..\packages\xunit.runner.visualstudio.2.3.0\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert.2.3.0\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core.2.3.0\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution.2.3.0\lib\net452\xunit.execution.desktop.dll"
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

    type Tree = Leaf of int | Branch of Tree * Tree

    let rec unsafeTree() = 
      Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                  Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

    let tree =
        let rec tree' s = 
            match s with
            | 0 -> Gen.map Leaf Arb.generate<int>
            | n when n>0 -> 
                let subtree = tree' (n/2)
                Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                            Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
            | _ -> invalidArg "s" "Only positive arguments are allowed"
        Gen.sized tree'

    type MyGenerators =
      static member Tree() =
          {new Arbitrary<Tree>() with
              override x.Generator = tree
              override x.Shrinker t = Seq.empty }
    
    Arb.register<MyGenerators>() |> ignore

    [<Property;Trait("FsCheck","Generator")>]
    let revRevTree (xs:list<Tree>) = 
      List.rev(List.rev xs) = xs
    // Check.Quick revRevTree

    type Box<'a> = Whitebox of 'a | Blackbox of 'a

    let boxGen<'a> : Gen<Box<'a>> = 
        gen { let! a = Arb.generate<'a>
              return! Gen.elements [ Whitebox a; Blackbox a] }

    type MyTreeGenerator =
        static member Tree() =
            {new Arbitrary<Tree>() with
                override x.Generator = tree
                override x.Shrinker t = Seq.empty }
        static member Box() = Arb.fromGen boxGen

    [<Property;Trait("FsCheck","Generator")>]
    let revRevBox (xs:list<Box<int>>) = 
      List.rev(List.rev xs) = xs
    // Check.Quick revRevBox

    type StringFloat = StringFloat of string

    type StringFloatGenerator =
        static member StringFloat () =
            Arb.generate<float>
            // FParsec can't handle infinities, nan
            // I'd use NormalFloat but I need to round-trip
            |> Gen.filter (fun x -> (x <> infinity && x <> -infinity && x <> nan) )
            |> Gen.map (fun x -> StringFloat (x.ToString("G17")))
            // Sometimes imposssible to produce a rountrippable float?
            |> Gen.filter (fun x -> (x <> StringFloat "NaN") )
            |> Arb.fromGen

//    Arb.register<StringFloatGenerator>() |> ignore

    let severalStringFloats = Gen.sample 100 10 Arb.generate<StringFloat>

    [<Property( Arbitrary=[| typeof<StringFloatGenerator> |])>]
    [<Trait("FsCheck","Generator")>]
    let ``stringFloat gen``(x:StringFloat) = 
        let (StringFloat str) = x
        let expected = System.Double.Parse(str)
        let actual = parse pfloat str
        expected = actual

    Check.Quick ``stringFloat gen``

(*
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        // NUnitLite if compiled to an exe https://github.com/nunit/docs/wiki/Installation
        // Can also just run NUnit.Console and run against the compiled dll https://github.com/nunit/docs/wiki/Console-Runner
        AutoRun().Execute(argv) // return an integer exit code
*)