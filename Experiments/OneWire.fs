namespace fSharpExperiments

module OneWire = 
    open System
    open System.IO
    open System.Text.RegularExpressions

    let getDeviceTemp () = 
    
        let devices = Directory.GetDirectories("/sys/bus/w1/devices")
                     |> Seq.filter(fun x -> not (String.Equals(x, "/sys/bus/w1/devices/w1_bus_master1")))

        devices |> Seq.map (fun x -> 
                                    let data = File.ReadAllLines(x + "/w1_slave")
                                    let m =   Regex.Match(data.[1],"""t=(\d+)$""")
                                    let r = (float (m.Groups.[1].Value))/1000.0
                                    let p = x.Split(Path.DirectorySeparatorChar)
                                    p.[p.Length - 1], r)

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

    [<EntryPoint>]
    let main argv = 
        getDeviceTemp ()
        0 // return an integer exit code
    
