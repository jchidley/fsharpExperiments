namespace OneWire

module CommandLine =
/// based on https://fsharpforfunandprofit.com/posts/pattern-matching-command-line/ - CommandLineV2
    type TimeoutOption = string
    type OneWireDirectory = string

    type CommandLineOptions = {
        owDirectory: OneWireDirectory;
        timeout: TimeoutOption
        }

    let printHelp () =
        printfn """
        Allowed options include:
        -d OneWireDirectory - where owDirectory is the path to the "owfs" defined file system.
        -t TimeoutInSeconds - a positive integer timeout
        -h or --help - print this text
        """

    // create the "helper" recursive function
    let rec parseCommandLineRec args optionsSoFar = 
        match args with 
        // empty list means we're done.
        | [] -> 
            optionsSoFar  

        | "-h"::xs | "--help"::xs -> 
            printHelp()
            //start a submatch on the next arg
            parseCommandLineRec xs optionsSoFar 

        // match directory flag
        | "/d"::xs -> 
            //start a submatch on the next arg
            match xs with
            | d::xss ->
                match System.IO.Directory.Exists(d) with
                | true -> 
                    let newOptionsSoFar = { optionsSoFar with owDirectory= d}
                    parseCommandLineRec xss newOptionsSoFar 
                | false -> 
                    printfn "valid directory needed"
                    parseCommandLineRec xss optionsSoFar 
            // handle unrecognized option and keep looping
            | _ -> 
                printfn "Need a directory after /d flag"
                parseCommandLineRec xs optionsSoFar 

        // match timeout flag
        | "/t"::xs -> 
            //start a submatch on the next arg
            match xs with
            | t::xss ->
                match System.Int32.TryParse(t) with
                | true, v -> 
                    let newOptionsSoFar = { optionsSoFar with timeout=t}
                    parseCommandLineRec xss newOptionsSoFar 
                | false, _ -> 
                    printfn "timeout in seconds needed"
                    parseCommandLineRec xss optionsSoFar 
            // handle unrecognized option and keep looping
            | _ -> 
                printfn "Need a timeout in seconds after /t flag"
                parseCommandLineRec xs optionsSoFar 

        // handle unrecognized option and keep looping
        | x::xs -> 
            printfn "Option '%s' is unrecognized" x
            parseCommandLineRec xs optionsSoFar 

    // create the "public" parse function
    let parseCommandLine argv = 
        let args = Array.toList argv
        // create the defaults
        let defaultOptions = {
            owDirectory = "/mnt/1wire";
            timeout = "120"
            }

        // call the recursive one with the initial options
        parseCommandLineRec args defaultOptions 
            

module MySqlds18b20 = 
    open FSharp.Data.Sql
    open System.IO
    open CommandLine

    // 28 is for DS18B20 thermometers.  
    // For remote @"\\piUSBarch\jack\1wire" or local "/mnt/1wire" or "/home/jack/1wire"

    // I thought that this was going to be straigtforward.  How wrong I was.
    // http://owfs.org/index.php?page=what-is-uncached

    let setup (options:CommandLineOptions) =
        File.WriteAllText(Path.Combine [|options.owDirectory;"settings";"timeout";"presence";|],options.timeout)
        File.WriteAllText(Path.Combine [|options.owDirectory;"settings";"timeout";"directory";|],options.timeout)
        File.WriteAllText(Path.Combine [|options.owDirectory;"settings";"timeout";"volatile";|],options.timeout)

    let devices owdirectory = Directory.GetDirectories(owdirectory,"28.*")

    let readData device dataToRead =
        File.ReadAllText (Path.Combine [|device;dataToRead|])
    
    let readTemperatureData device =
        let bytes = File.ReadAllBytes (Path.Combine [|device;"scratchpad"|])
        // let bytes = [| byte 0x90 ; byte 0xFC|] // -25.0625 expected
        // http://fssnip.net/Q Bit manipulation methods
        let reading = int16 (int bytes.[0]  + (int bytes.[1] <<< 8))
        float32 (float reading / 16.0)

    [<Literal>]
    let connString  = "Server=pi3debian1;Database=turd;User=jack;Password=kJyoZYyRB0Iv0JmfZ7tq"
    [<Literal>]
    let dbVendor    = Common.DatabaseProviderTypes.MYSQL
    [<Literal>]
    // let resPath = @"C:\Users\jackc\Documents\Git\fsharpExperiments\packages\MySql.Data\lib\net452" // net 452
    let resPath = "connectorTemp" // net core 2.0
    [<Literal>]
    let indivAmount = 1000
    [<Literal>]
    let useOptTypes = true

    type sql = SqlDataProvider<
                    dbVendor,
                    connString,
                    ResolutionPath = resPath,
                    IndividualsAmount = indivAmount,
                    UseOptionTypes = useOptTypes
                >
    let ctx = sql.GetDataContext()

    let ds18b20 = ctx.Turd.Ds18b20

    let updateReadings (options:CommandLineOptions) =
        
        let a = async { do setup options
                        File.WriteAllText(Path.Combine [|options.owDirectory;"simultaneous";"temperature"|],"1")  // Simulataneous read
                        do! Async.Sleep(1000) // Wait max time before starting to read, it is apparently 750ms
                        do Directory.GetDirectories(options.owDirectory) |> ignore // odd bug where files and directories disappear

                        let updates = devices options.owDirectory |> Array.map (fun d -> 
                            let now = System.DateTime.Now
                            let id = readData d "r_id"
                            let temp = readTemperatureData d
                            printfn "%A %s %A" now id temp
                            ctx.Turd.Ds18b20.``Create(datetime, sensor, temperature)``(now, id, temp) )
                        ctx.SubmitUpdates()
                      }
        
        Async.RunSynchronously a

        printfn "sent data at %s" (System.DateTime.Now.ToShortTimeString())

(*
module Tests = 
    open CommandLine
    open MySqlds18b20

    // happy path
    parseCommandLine [|"/d"; "c:\\"|] |> ignore
    parseCommandLine [|"/t"; "60"|] |> ignore
    parseCommandLine [|"/d"; @"\\piUSBdebian\jack\1wire"|] |> ignore

    // error handling
    parseCommandLine [|"/v"; "xyz"|] |> ignore
    parseCommandLine [|"/o"; "xyz"|] |> ignore
*)