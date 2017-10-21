#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
#endif

module FsCheckTest
    // open NUnit.Framework
    open Xunit
    open FsCheck
    open FsCheck.Xunit

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

(*
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        // NUnitLite if compiled to an exe https://github.com/nunit/docs/wiki/Installation
        // Can also just run NUnit.Console and run against the compiled dll https://github.com/nunit/docs/wiki/Console-Runner
        AutoRun().Execute(argv) // return an integer exit code
*)