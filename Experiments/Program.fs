open OneWire
open MySqlds18b20
open CommandLine

    [<EntryPoint>]
    let main argv = 
        printfn "Starting at %s" (System.DateTime.Now.ToShortTimeString())
        let options = parseCommandLine argv
        let sleepTimer = (int options.timeout * 1000) - 30 * 1000
        let rec tempLoop () = async {
                                    MySqlds18b20.updateReadings(options)
                                    do! Async.Sleep(sleepTimer)
                                    return! tempLoop()
                                    }

        Async.RunSynchronously (tempLoop())
        0 // return an integer exit code
