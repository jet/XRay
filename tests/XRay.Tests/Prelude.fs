namespace XRay.Tests

open XRay

module Prelude =

    /// Dummy logger
    let ignoreLogger (_: string) = ()

    /// A codec between a type 'a and a byte array.
    type ByteCodec<'a> = Codec<byte [], 'a>
    
    open System.IO
    open FSharp.Data

    type JsonValue with
        static member toBytes (json: JsonValue) = (
            use ms = new MemoryStream ()
            (
                use w = new StreamWriter (ms)
                json.WriteTo (w, JsonSaveOptions.DisableFormatting)
                w.Flush ()
                ms.ToArray ()))
        static member ofBytes (data: byte []) = 
            let UTF8NoBOM = new System.Text.UTF8Encoding (false, true)
            JsonValue.Parse (UTF8NoBOM.GetString data)

    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    let private s = new JsonSerializer () 

    type JToken with
        static member toBytes (o: JToken) =
            use ms = new MemoryStream ()
            (
                use jsonWriter = new JsonTextWriter (new StreamWriter (ms))
                s.Serialize (jsonWriter, o))
            ms.ToArray ()
        static member ofBytes (data: byte []) =
            use ms = new MemoryStream (data)
            use jsonReader = new JsonTextReader (new StreamReader (ms))
            s.Deserialize (jsonReader, typeof<JToken>) :?> JToken

    type JsonValueFd = JsonValue
    type JsonValueNs = JToken


    ////////////////////////////////////////////////
    // Define JsonFromFSharpData using FSharpData //
    ////////////////////////////////////////////////
    
    open Fleece.FSharpData.Lens
    let JsonFromFSharpData = XRay.Codec.Json<JsonValueFd> (preview << _jkey, setl << _jkey, Fleece.FSharpData.JNull)


    ////////////////////////////////////////////////
    // Define JsonFromNewtonsoft using Newtonsoft //
    ////////////////////////////////////////////////

    open Fleece.Newtonsoft.Lens
    let JsonFromNewtonsoft = XRay.Codec.Json<JsonValueNs> (preview << _jkey, setl << _jkey, Fleece.Newtonsoft.JNull)


    
    ///////////////////////
    // Create a CtxCodec //
    ///////////////////////

    open FSharpPlus    

    module CtxCodecFSharpData =
        open Fleece.FSharpData
        open Fleece.FSharpData.Operators
    
        let decodeTraceCtx (js: JsonValueFd) : ParseResult<TraceContext> =
            match js with
            | JObject o -> monad {
                let! trace_id   =  o .@ "tid"
                let! trace_tags = (o .@ "ts") |> Result.map TraceTags.ofMap
                return {
                    TraceId = trace_id
                    Tags = trace_tags }}
            | x -> Helpers.failparse "TraceContext" x
    
        let encodeTraceCtx (t: TraceContext) : JsonValueFd =
            jobj [|
                jpair "tid" t.TraceId
                jpair "ts" (TraceTags.toMap t.Tags)
            |]
        
        let jsonValueCodecT = decodeTraceCtx, encodeTraceCtx


    module CtxCodecNewtonsoft =
        open Fleece.Newtonsoft
        open Fleece.Newtonsoft.Operators

        let decodeTraceCtx (js: JsonValueNs) : ParseResult<TraceContext> =
            match js with
            | JObject o -> monad {
                let! trace_id   =  o .@ "tid"
                let! trace_tags = (o .@ "ts") |> Result.map TraceTags.ofMap
                return {
                    TraceId = trace_id
                    Tags = trace_tags }}
            | x -> Helpers.failparse "TraceContext" x
    
        let encodeTraceCtx (t: TraceContext) : JsonValueNs =
            jobj [|
                jpair "tid" t.TraceId
                jpair "ts" (TraceTags.toMap t.Tags)
            |]
        
        let jsonValueCodecT = decodeTraceCtx, encodeTraceCtx
    

    /////////////////////////////
    // Create a sample message //
    /////////////////////////////

    type Message = { message: string }

    open Fleece.FSharpData
    open Fleece.FSharpData.Operators
    type Message with
        static member OfJson json =
            match json with
            | JObject o -> Result.map (fun x -> {message = x}) (o .@ "message")
            | x -> Helpers.failparse "Message" x
        static member ToJSON (m: Message) = jobj [| jpair "message" m.message |]

    open Fleece.Newtonsoft
    open Fleece.Newtonsoft.Operators
    type Message with
        static member OfJson json =
            match json with
            | JObject o -> Result.map (fun x -> {message = x}) (o .@ "message")
            | x -> Helpers.failparse "Message" x
        static member ToJson (m: Message) = jobj [| jpair "message" m.message |]


    //////////////////////////////////////////////////////////////
    // Define JsonFromFSharpDataCtx using Fleece (w/FSharpData) //
    //////////////////////////////////////////////////////////////

    open Fleece.FSharpData
    open Fleece.FSharpData.Lens

    /// A codec for FSharp.Data's JsonValue.
    type JsonValueCodecFSharpData<'a> = Codec<JsonValueFd, 'a>
    
    /// Operations on JsonValue codecs.
    module JsonValueCodecFSharpData =        
        let toBytes (codec: JsonValueCodecFSharpData<'a>) : ByteCodec<'a> = Fleece.FSharpData.Codec.invmap JsonValueFd.ofBytes JsonValueFd.toBytes codec
        let inline jsonValueCodecT () : JsonValueCodecFSharpData<_> = (ofJson, toJson)

    let JsonFromFSharpDataCtx = XRay.Codec.JsonCtx<JsonValueFd> (preview << _jkey, setl << _jkey, JNull, CtxCodecFSharpData.jsonValueCodecT)


    //////////////////////////////////////////////////////////////
    // Define JsonFromNewtonsoftCtx using Fleece (w/Newtonsoft) //
    //////////////////////////////////////////////////////////////

    open Fleece.Newtonsoft
    open Fleece.Newtonsoft.Lens

    /// A codec for Newtonsoft's JToken.
    type JsonValueCodecNewtonsoft<'a> = Codec<JsonValueNs, 'a>
    
    /// Operations on JsonValue codecs.
    module JsonValueCodecNewtonsoft =        
        let toBytes (codec: JsonValueCodecNewtonsoft<'a>) : ByteCodec<'a> = Fleece.Newtonsoft.Codec.invmap JsonValueNs.ofBytes JsonValueNs.toBytes codec
        let inline jsonValueCodecT () : JsonValueCodecNewtonsoft<_> = (ofJson, toJson)

    let JsonFromNewtonsoftCtx = XRay.Codec.JsonCtx<JsonValueNs> (preview << _jkey, setl << _jkey, JNull, CtxCodecNewtonsoft.jsonValueCodecT)


    /////////////////////////////
    // Telemetry Event decoder //
    /////////////////////////////

    let telemetryEventDecNs (m: TelemetryEvent) = 
        ([|
            jpairWith  JsonEncode.dateTimeOffset         "ts"   m.Timestamp
            jpairWith  JsonEncode.string                 "et"   m.EventType
            jpairWith  JsonEncode.string                 "tid"  m.TraceId
            jpairWith (JsonEncode.map JsonEncode.string) "tags" (TraceTags.toMap m.Tags)
        |] |> jobj).ToString Newtonsoft.Json.Formatting.None

    let telemetryEventWriter (x: TelemetryEvent) (sb: System.Text.StringBuilder) =
        sb.Append (telemetryEventDecNs x) |> ignore