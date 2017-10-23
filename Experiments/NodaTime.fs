#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\Newtonsoft.Json.9.0.1\lib\net45\Newtonsoft.Json.dll"
    #r @"..\packages\NodaTime.2.2.1\lib\net45\NodaTime.dll"
    #r @"..\packages\NodaTime.Serialization.JsonNet.2.0.0\lib\net45\NodaTime.Serialization.JsonNet.dll"
    #r @"..\packages\NodaTime.Testing.2.2.1\lib\net45\NodaTime.Testing.dll"
    #r @"..\packages\NUnit.3.8.1\lib\net45\nunit.framework.dll"
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
    #r @"..\packages\xunit.runner.visualstudio.2.3.0\build\net20\..\_common\xunit.abstractions.dll"
    #r @"..\packages\xunit.assert.2.3.0\lib\netstandard1.1\xunit.assert.dll"
    #r @"..\packages\xunit.extensibility.core.2.3.0\lib\netstandard1.1\xunit.core.dll"
    #r @"..\packages\xunit.extensibility.execution.2.3.0\lib\net452\xunit.execution.desktop.dll"
    #r @"..\packages\FsCheck.Xunit.2.10.3\lib\net452\FsCheck.Xunit.dll"
#endif

namespace fSharpExperiments

module getReferences =
    open System.Text.RegularExpressions

    let src = """--> Referenced 'C:\\Users\\jackc\\Documents\\Git\\fsharpExperiments"""
    let repl = """#r @".."""
    let replaceIt (str:string) = Regex.Replace(str,src,repl)

    let str = """

--> Referenced 'C:\Users\jackc\Documents\Git\fsharpExperiments\packages\FsCheck.Xunit.2.10.3\lib\net452\FsCheck.Xunit.dll'


"""

    let newS = replaceIt str

module NodaExperiments = 
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open NodaTime
    open NodaTime.Serialization.JsonNet
    open Newtonsoft.Json
    open Newtonsoft.Json.Converters
    open System.Collections.Generic

    let now = NodaTime.SystemClock.Instance.GetCurrentInstant()

    let birthDate = LocalDate(1967, 9, 16)
    let zone = DateTimeZoneProviders.Tzdb.["Europe/London"]
    let clock = NodaTime.ZonedClock(NodaTime.SystemClock.Instance, zone, NodaTime.CalendarSystem.Iso)
    let today = clock.GetCurrentDate()
    let age = Period.Between(birthDate, today)
    printfn "Jack is %d years %d months and %d days old" age.Years age.Months age.Days
    
    NodaTime.Calendars.WeekYearRules.Iso.GetWeekOfWeekYear(today) |> ignore

    birthDate.ToString("d", System.Globalization.CultureInfo.CurrentCulture) |> ignore
    
    let dateTime = new System.DateTime(2012, 1, 2, 3, 4, 5, System.DateTimeKind.Utc)
    let instant = Instant.FromDateTimeUtc(dateTime)

    let jsonDateTime = JsonConvert.SerializeObject(dateTime, new IsoDateTimeConverter())
    let jsonInstant = JsonConvert.SerializeObject(instant, Formatting.None, NodaConverters.InstantConverter)

    [<Fact>]
    let ``We can round trip NodaTime Durations using Json`` () =
        let a = JsonConvert.DeserializeObject<Duration>("\"25:10:00.1234000\"", NodaConverters.DurationConverter) 
        let b = (Duration.FromHours(25) + Duration.FromMinutes(10.0) + Duration.FromTicks(1234000.0))
        Assert.Equal(a,b)

    let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
    Check.Quick revRevIsOrig

    type Generators =
        static member Instant() =
            Arb.generate<System.DateTime>
            |> Gen.map (fun dt -> dt.ToUniversalTime())
            |> Gen.map (fun dt -> Instant.FromDateTimeUtc dt)
            |> Arb.fromGen
    
    Arb.register<Generators>() |> ignore 

    [<Property ( Arbitrary=[| typeof<Generators> |] )>]
    let ``NodaTime Instants can be round-tripped using JSON`` (x) = 
            let jsonInstant = JsonConvert.SerializeObject(x, Formatting.None, NodaConverters.InstantConverter)
            let settings = new JsonSerializerSettings( 
                                        Converters =  ([| NodaConverters.InstantConverter |] :> IList<JsonConverter>),
                                        DateParseHandling = DateParseHandling.None )
            x = JsonConvert.DeserializeObject<Instant>(jsonInstant, settings)

