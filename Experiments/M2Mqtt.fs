(*
#if INTERACTIVE 
    #I __SOURCE_DIRECTORY__
    #r @"..\packages\M2Mqtt\lib\net45\M2Mqtt.Net.dll"
    #r @"C:\Users\jackc\Documents\Git\StriderMqtt\StriderMqtt\bin\Debug\StriderMqtt.dll"
#endif
*)

namespace fSharpExperiments

module Mqtt = 

    /// Used in the Microframework, probably pretty lightweight
    open uPLibrary.Networking.M2Mqtt
    open uPLibrary.Networking.M2Mqtt.Messages
    open System

    // create client instance
    let client = new MqttClient("pi3debian1" )

    let clientId = Guid.NewGuid().ToString();
    do client.Connect(clientId) |> ignore 

    // register to message received 
    do client.MqttMsgPublishReceived.Add(fun e -> printfn "%s %s" e.Topic  (Text.Encoding.UTF8.GetString(e.Message) )) |> ignore

    // subscribe to the topic "/home/temperature" with QoS 2
    do client.Subscribe([|"+/office/#"|], [|MqttMsgBase.QOS_LEVEL_EXACTLY_ONCE|]) |> ignore 
    do client.Subscribe([|"+/wemosd1pro/#"|], [|MqttMsgBase.QOS_LEVEL_EXACTLY_ONCE|]) |> ignore 

    /// More recently developed
    open StriderMqtt

    let connArgs = new MqttConnectionArgs(ClientId = "my-strider-client",
                                                Hostname = "pi3debian1",
                                                Port = 1883)

    let sendIt connArgs topic message = 
        use conn = new MqttConnection(connArgs)
        conn.Connect()
        conn.Publish(topic,
                        System.Text.Encoding.UTF8.GetBytes(message:string),
                        MqttQos.AtMostOnce) |> ignore
        conn.Disconnect()

    sendIt connArgs "cmnd/office/power" "toggle"

(*
let m2m = @"C:\Users\jackc\Documents\Git\fsharpExperiments\packages\M2Mqtt\lib\net45\M2Mqtt.Net.dll"
let strider = @"C:\Users\jackc\Documents\Git\StriderMqtt\StriderMqtt\bin\Debug\StriderMqtt.dll"

let useInMono r = 
    let x = System.Text.RegularExpressions.Regex.Replace(r, @"\\", "/")
    System.Text.RegularExpressions.Regex.Replace(x, @"C:", "/mnt/c")

[strider; m2m] 
|> List.map useInMono
|> List.map (fun x -> printfn "#r \"%s\"" x)

[@"C:\Users\jackc\Documents\Git\fsharpExperiments\Experiments\M2Mqtt.fs"] 
|> List.map useInMono
|> List.map (fun x -> printfn "#load \"%s\"" x)
*)