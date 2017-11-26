let one = lazy (printfn "one")
let two () = printfn "two"
let three = printfn "three"

two()
one.Force()
three
two()
one.Force()

// modified http://www.fssnip.net/y/title/Using-the-lazy-Keyword
let lazySideEffect =
    lazy
        ( let temp = 2 + 2
          printfn "%i" temp
          temp )
          
printfn "Force value the first time: "
let actualValue1 = lazySideEffect.Force()
printfn "Force value the second time: "
let actualValue2 = lazySideEffect.Force()

// from https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/lazy.force%5B%27t%5D-extension-method-%5Bfsharp%5D
let lazyFactorial n = Lazy.Create (fun () ->
    let rec factorial n =
        match n with
        | 0 | 1 -> 1
        | n -> n * factorial (n - 1)
    factorial n)
let printLazy (lazyVal:Lazy<int>) =
    if (lazyVal.IsValueCreated) then
        printfn "Retrieving stored value: %d" (lazyVal.Value)
    else
        printfn "Computing value: %d" (lazyVal.Force())
let lazyVal1 = lazyFactorial 12
let lazyVal2 = lazyFactorial 10
lazyVal1.Force() |> ignore
printLazy lazyVal1
printLazy lazyVal2
