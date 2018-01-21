#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\xunit.runner.visualstudio\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution\lib\net452\xunit.execution.desktop.dll"
    #r @"..\packages\FParsec\lib\net40-client\FParsecCS.dll"
    #r @"..\packages\FParsec\lib\net40-client\FParsec.dll"
#endif

module fSharpExperiments.DHCPParse =
    open System
    open System.Diagnostics

    let private runCommand cmd args =
        let startInfo = new ProcessStartInfo()
        startInfo.FileName <- cmd
        startInfo.Arguments <- args
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true

        let proc = Process.Start(startInfo)
        use stream = proc.StandardOutput
        stream.ReadToEnd()

    let log branch args =
        let args = sprintf "log %s %s" branch (String.concat " " args)
        runCommand "git" args


[<AutoOpen>]
module Types = 
    open System
    // source "Jan 07 12:01:43 archShuttle1 dhcpd[1593]: DHCPACK on 192.168.99.101 to 00:1e:2a:4a:e2:5e (DESKTOP-U43D05N) via LAN"

    type EntryDateTime = EntryDateTime of string

    type DhcpServerName = DhcpServerName of string

    type DhcpKey = DhcpKey of string

    type DhcpMsg = DhcpMsg of string

    // "on"

    type SourceIp = SourceIp of string

    // "to"

    type ArpAdd = ArpAdd of string

    type HostName = HostName of string


    // "via"

    type NetworkIface = NetworkIface of string

    type IpAssignment = {
        SourceIp:SourceIp
        ArpAdd:ArpAdd
        HostName:HostName}

module DhcpLogParser =
    open System
    open FParsec
    open fSharpExperiments.FParsecFloatTest

    let private parser =
        let str_ws s = pstring s .>> spaces
        let char_ws c = pchar c .>> spaces
        let ws_char c = spaces >>. pchar c
        let puint8_ws s = puint8 .>> spaces
        let anyCharsTill pEnd = manyCharsTill anyChar pEnd
        let line = anyCharsTill newline
        test line ("Jan \n")
        let restOfLineAfter str = str_ws str >>. line

        let entryDateTime =
            let d = test line "Jan \n"
            parse puint8_ws  "Jan 07 12:01:43"
        let dhcpServerName = "archShuttle1"
        let dhcpKey = "dhcpd[1593]:"
        let dhcpMsg = "DHCPACK"
    // "on"
        let sourceIp = "192.168.99.101"
    // "to"
        let arpAdd = "00:1e:2a:4a:e2:5e"
        let hostName = "(DESKTOP-U43D05N)"
    // "via"
        let networkIface = "LAN"

        let ipAssignment = {
                            SourceIp = SourceIp sourceIp
                            ArpAdd = ArpAdd arpAdd
                            HostName = Hostname HostName }

        let id = restOfLineAfter "commit"
        let date = restOfLineAfter "Date:"
        let merge = restOfLineAfter "Merge:"
        let email = ws_char '<' >>. anyCharsTill (char_ws '>')
        let name = anyCharsTill (lookAhead email)
        let author = str_ws "Author:" >>. name .>>. email
        let msgLine = spaces >>. line
        let msg = manyTill msgLine (lookAhead (newline >>. id) |>> ignore <|> eof)

        let commitId = (spaces >>. id .>> optional merge)

        let createCommit id (name, email) date msg = {
            Id      = Id id
            Author  = { Name = name; Email = email }
            Date    = DateTimeOffset.Parse(date)
            Message = Types.Message (String.concat Environment.NewLine msg) }

        let commit = pipe4 commitId author date  msg createCommit

        many commit .>> eof

    let parseLog log =
        match log |> run parser with
        | Success(v,_,_)   -> v
        | Failure(msg,_,_) -> failwith msg

/// https://gist.githubusercontent.com/battermann/123f340c5b93fa08a67c2093f8fe81b0/raw/3fd773fff0664b3509df7b98cbc4aee01a70028c/gitlogparser.fsx
/// http://blog.leifbattermann.de/2016/08/11/how-to-parse-a-git-log-with-fparsec/

module Git =
    open System
    open System.Diagnostics

    let private runCommand cmd args =
        let startInfo = new ProcessStartInfo()
        startInfo.FileName <- cmd
        startInfo.Arguments <- args
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true

        let proc = Process.Start(startInfo)
        use stream = proc.StandardOutput
        stream.ReadToEnd()

    let log branch args =
        let args = sprintf "log %s %s" branch (String.concat " " args)
        runCommand "git" args

[<AutoOpen>]
module Types = 
    open System

    type Id = Id of string

    type Message = Message of string

    type Author = {
        Name:string
        Email:string }

    type Commit = {
        Id:Id
        Author:Author
        Date:DateTimeOffset
        Message:Message }

module GitLogParser =
    open System
    open FParsec

    let private parser =
        let str_ws s = pstring s .>> spaces
        let char_ws c = pchar c .>> spaces
        let ws_char c = spaces >>. pchar c
        let anyCharsTill pEnd = manyCharsTill anyChar pEnd
        let line = anyCharsTill newline
        let restOfLineAfter str = str_ws str >>. line

        let id = restOfLineAfter "commit"
        let date = restOfLineAfter "Date:"
        let merge = restOfLineAfter "Merge:"
        let email = ws_char '<' >>. anyCharsTill (char_ws '>')
        let name = anyCharsTill (lookAhead email)
        let author = str_ws "Author:" >>. name .>>. email
        let msgLine = spaces >>. line
        let msg = manyTill msgLine (lookAhead (newline >>. id) |>> ignore <|> eof)

        let commitId = (spaces >>. id .>> optional merge)

        let createCommit id (name, email) date msg = {
            Id      = Id id
            Author  = { Name = name; Email = email }
            Date    = DateTimeOffset.Parse(date)
            Message = Types.Message (String.concat Environment.NewLine msg) }

        let commit = pipe4 commitId author date  msg createCommit

        many commit .>> eof

    let parseLog log =
        match log |> run parser with
        | Success(v,_,_)   -> v
        | Failure(msg,_,_) -> failwith msg

open System
        
let run branch  = 

    let averageMsgLength = List.map (fun c -> c.Message) >> List.averageBy (fun (Message m) -> float m.Length)

    let partitionCommitsByPartOfDay = List.countBy (fun c -> 
        let within start stop (ts:TimeSpan) = ts.Hours >= start && ts.Hours < stop
        let morning = within 6 10
        let daytime = within 10 17
        let evening = within 17 22

        if morning c.Date.TimeOfDay then "Morning"
        else if daytime c.Date.TimeOfDay then "Daytime"
        else if evening c.Date.TimeOfDay then "Evening"
        else "Overnight" )

    let print (name, count, length, stats) =
        do printfn "%s" name
        do printfn "\tTotal commits: %d" count
        do printfn "\tCommits by part of day:%s%s" Environment.NewLine 
            (stats 
             |> List.sortBy snd 
             |> List.rev 
             |> List.map (fun (key, n) -> sprintf "\t\t%s: %.0f %%" key (float n / float count * 100.0))
             |> String.concat Environment.NewLine)
        do printfn "\tAverage commit message size: %.0f" length

    let commits = 
        Git.log branch ["--date iso"]
        |> GitLogParser.parseLog

    commits
    |> List.groupBy (fun c -> c.Author.Name)
    |> List.map (fun (key, xs) -> key, xs.Length, averageMsgLength xs, partitionCommitsByPartOfDay xs)
    |> List.sortBy (fun (_,commits,_,_) -> commits)
    |> List.rev
    |> List.take 5
    |> List.iter print

run "master"
