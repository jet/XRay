#I "../bin/Debug/net451"

#r "XRay.dll"
#load "Prelude.fsx"

open System.Net
open XRay
open Prelude

(*

This script provides an example of using Xray to trace the interaction between two
HTTP services - service A and B. Service A calls service B as part of performing its
operation and propagates the trace.

*)


// Inject Codec (Using Fleece - Newtonsoft Json Library)

open Fleece.Newtonsoft
let CodecHttp =
    CtxCodecNewtonsoft.jsonValueCodecT
    |> Codec.compose jsonValueToTextCodec
    |> Codec.Http

open System

let telemetryEventDecoder (m: TelemetryEvent) =
    ([|
        jpairWith  JsonEncode.dateTimeOffset         "ts"   m.Timestamp
        jpairWith  JsonEncode.string                 "et"   m.EventType
        jpairWith  JsonEncode.string                 "tid"  m.TraceId
        jpairWith (JsonEncode.map JsonEncode.string) "tags" (TraceTags.toMap m.Tags)
    |] |> jobj).ToString Newtonsoft.Json.Formatting.None

let te = {
    TraceId = "42"
    EventType = ""
    Timestamp = DateTimeOffset.MinValue
    Tags = TraceTags.ofSeq [ "e\"scape","the" ; "special", "chars" ] }

let sb = System.Text.StringBuilder ()
let telemetryEventWriter (x: TelemetryEvent) (sb: System.Text.StringBuilder) =
    sb.Append (telemetryEventDecoder x) |> ignore
telemetryEventWriter te sb


/// This is the code for service A
let serviceA (host:string, serviceBHost:string)  = async {

  // create the Xray log
  let xray = TracePublisher.createDefault "msku_service" (fun x -> printfn "%s" x) telemetryEventWriter

  // the name of the operation performed by this service
  let serverSpanName = "import_msku"

  // the name of the client operation
  let clientSpanName = "get_sku_info"

  // client
  let serviceB =
    HttpClient.baseAddress serviceBHost    
  
  // handles HTTP requests
  let handle (req:HttpReq) : Async<HttpRes> = async {

    // start a (server) span by first attempting to decode a trace context from the incoming request.
    let req,span = req |> Span.decodeStart xray id serverSpanName (CodecHttp.Req |> Codec.decoder) 
    logInfo "received_request|trace_id=%s" span.TraceId

    // start client call sequence -----------------------------------------------------------
    logInfo "calling_service|caller=serviceA callee=serviceB"

    // construct a request to the client
    let clientReq = HttpReq.post () |> HttpReq.withBodyString "hello B"

    // start a client span and embed the trace into the client request, thereby propagating it
    let clientSpan,clientReq =
      clientReq |> Span.startPropagate xray id span clientSpanName (CodecHttp.Req |> Codec.encoder)

    // call the client for service B
    let! res' = serviceB clientReq
    
    // complete the client span
    let res',clientTraceContext =
      Span.completeAck xray clientSpan (CodecHttp.Res |> Codec.decoder) res'

    logInfo "calling_service_completed|caller=serviceA callee=serviceB"

    // end client call sequence ---------------------------------------------------

    // construct the server response
    let res =
      HttpRes.ofStatusCode HttpStatusCode.OK
      |> HttpRes.withBodyStringUTF8 "hello" "text/plain"

    // complete the server span and embed span completion data into the response.
    let res = Span.completeEcho xray span (CodecHttp.Res |> Codec.encoder) res

    return res }
    
  do! HttpHost.hostAsync host handle

}


/// This is the code for serviceB
let serviceB (host:string)  = async {

  let xray = TracePublisher.createDefault "sku_service" (fun x -> printfn "%s" x) telemetryEventWriter

  let serverSpanName = "import_msku"

  // handles HTTP requests
  let handle (req:HttpReq) : Async<HttpRes> = async {

    let req,span = req |> Span.decodeStart xray id serverSpanName (CodecHttp.Req |> Codec.decoder) 

    logInfo "received_request|service=serviceB trace_id=%s op=%s" span.TraceId "op_b"
    
    do! Async.Sleep 1000 // TODO: service logic

    let res =
      HttpRes.ofStatusCode HttpStatusCode.OK
      |> HttpRes.withBodyStringUTF8 "hello" "text/plain"

    let res = Span.completeEcho xray span (CodecHttp.Res |> Codec.encoder) res

    logInfo "sending_response|service=serviceB trace_id=%s" span.TraceId

    return res }
    
  do! HttpHost.hostAsync host handle

}

let serviceAHost = "http://localhost:8080/"
let serviceBHost = "http://localhost:8081/"

Async.Parallel
  [ serviceA (serviceAHost, serviceBHost) ; serviceB serviceBHost ]
  |> Async.RunSynchronously