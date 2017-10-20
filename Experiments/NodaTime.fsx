#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\Newtonsoft.Json.9.0.1\lib\net45\Newtonsoft.Json.dll"
    #r @"..\packages\NodaTime.2.2.1\lib\net45\NodaTime.dll"
    #r @"..\packages\NodaTime.Serialization.JsonNet.2.0.0\lib\net45\NodaTime.Serialization.JsonNet.dll"
    #r @"..\packages\NodaTime.Testing.2.2.1\lib\net45\NodaTime.Testing.dll"
    #r @"..\packages\NUnit.3.8.1\lib\net45\nunit.framework.dll"
    #r @"..\packages\System.Globalization.Calendars.4.0.1\lib\net46\System.Globalization.Calendars.dll"
    #r @"..\packages\FsCheck.2.10.3\lib\net452\FsCheck.dll"
#endif

module getReferences = 
    open System.Text.RegularExpressions

    let src = """'\s+--> Referenced 'C:\\Users\\jackc\\source\\repos\\Experiments"""

    let repl = """"
#r @".."""

    let replaceIt (str:string) = Regex.Replace(str,src,repl)

(*
let str = """
'
--> Referenced 'C:\Users\jackc\source\repos\Experiments\packages\NodaTime.Testing.2.2.1\lib\net45\NodaTime.Testing.dll'


--> Referenced 'C:\Users\jackc\source\repos\Experiments\packages\NUnit.3.6.0\lib\net45\nunit.framework.dll'
"""

getReferences.replaceIt str
*)


module NodaExperiments = 

    open NodaTime

    let now = NodaTime.SystemClock.Instance.GetCurrentInstant()

    let birthDate = LocalDate(1967, 9, 16)
    let zone = DateTimeZoneProviders.Tzdb.["Europe/London"]
    let clock = NodaTime.ZonedClock(NodaTime.SystemClock.Instance, zone, NodaTime.CalendarSystem.Iso)
    let today = clock.GetCurrentDate()
    let age = Period.Between(birthDate, today)
    printfn "Jack is %d years %d months and %d days old" age.Years age.Months age.Days
    
    NodaTime.Calendars.WeekYearRules.Iso.GetWeekOfWeekYear(today)

    birthDate.ToString("d", System.Globalization.CultureInfo.CurrentCulture)
    
    open NodaTime.Serialization.JsonNet
    open Newtonsoft.Json
    open Newtonsoft.Json.Converters
    
    let dateTime = new System.DateTime(2012, 1, 2, 3, 4, 5, System.DateTimeKind.Utc)

    let instant = Instant.FromDateTimeUtc(dateTime)

    let jsonDateTime = JsonConvert.SerializeObject(dateTime, new IsoDateTimeConverter())
    let jsonInstant = JsonConvert.SerializeObject(instant, Formatting.None, NodaConverters.InstantConverter)
    
    open NUnit.Framework

    let a = JsonConvert.DeserializeObject<Duration>("\"25:10:00.1234000\"", NodaConverters.DurationConverter) 
    let b = (Duration.FromHours(25) + Duration.FromMinutes(10.0) + Duration.FromTicks(1234000.0))

    Assert.AreEqual(a,b)

//    https://neoeinstein.github.io/blog/2015/12-15-chiron-taming-types-in-the-wild/index.html 
    
    open System.Collections.Generic

    let settings = new JsonSerializerSettings( 
                        Converters =  ([| NodaConverters.InstantConverter |] :> IList<JsonConverter>),
                        DateParseHandling = DateParseHandling.None )
    Assert.AreEqual(instant, JsonConvert.DeserializeObject<Instant>(jsonInstant, settings))

    open FsCheck

    let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs


    Check.Quick revRevIsOrig

    let serialDeserialInstantIsOrig x =
        let jsonInstant = JsonConvert.SerializeObject(x, Formatting.None, NodaConverters.InstantConverter)
        let settings = new JsonSerializerSettings( 
                                    Converters =  ([| NodaConverters.InstantConverter |] :> IList<JsonConverter>),
                                    DateParseHandling = DateParseHandling.None )
        x = JsonConvert.DeserializeObject<Instant>(jsonInstant, settings)

    Check.Quick serialDeserialInstantIsOrig

    open FsCheck

    type NodaGen =
        static member Instant () =
            Arb.generate<System.DateTime>
            |> Gen.map (fun dt -> dt.ToUniversalTime())
            |> Gen.map (fun dt -> Instant.FromDateTimeUtc dt)
            |> Arb.fromGen

    // If using NUnit...
    // [<SetUp>]
    let setup () =
        do Arb.register<NodaGen>() |> ignore 


let intGenerator = Arb.generate<NodaGen>
let timeGenerator = Arb.generate<System.DateTime>
let instantGenerator = Arb.generate<Instant>

Gen.sample 0 10 instantGenerator


