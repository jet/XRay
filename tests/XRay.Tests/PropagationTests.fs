namespace XRay.Tests

open Xunit
open XRay
open XRay.Tests.Prelude
open FSharp.Data
open Newtonsoft.Json.Linq
open FSharpPlus

module PropagationTests =
    open System
  
    [<Fact>]
    let ``Trace header is generated when absent (FSharp.Data)``() =
    
        /// Maps over the input of the decoder and the output of the encoder.
        let dimap (f:'i2 -> 'i) (g:'o -> 'o2) (c:Codec<'i, 'o, 'a>) : Codec<'i2, 'o2, 'a> = let (dec,enc) = c in (f >> dec,enc >> g)
        let inline toBytes (c:JsonValueCodecFSharpData<'a>)  : ByteCodec<'a> = c |> dimap JsonValue.ofBytes JsonValue.toBytes
        
        let efs =
            JsonFromFSharpDataCtx.embeddedFieldDefault (JsonValueCodecFSharpData.jsonValueCodecT ())
            |> toBytes
        
        let dec = efs |> Codec.decoder
        let jsonStr = """{
            "message": "hello"
            }"""
        let decodeResult: Result<(Message*TraceContext), string> = jsonStr |> System.Text.Encoding.Default.GetBytes |> Dec.decode dec
        match decodeResult with
        | Error e -> failwith (sprintf "Parser failed : %s" e)
        | Ok (msg, trace) ->
            printfn "trace=%O" trace
            Assert.True(msg.message = "hello")
            Assert.NotNull(trace.TraceId)
        ()


    [<Fact>]
    let ``Trace header is generated when absent (Json.NET)``() =
    
        /// Maps over the input of the decoder and the output of the encoder.
        let dimap (f:'i2 -> 'i) (g:'o -> 'o2) (c:Codec<'i, 'o, 'a>) : Codec<'i2, 'o2, 'a> = let (dec,enc) = c in (f >> dec,enc >> g)
        let inline toBytes (c:JsonValueCodecNewtonsoft<'a>)  : ByteCodec<'a> = c |> dimap JToken.ofBytes JToken.toBytes
        
        let efs =
            JsonFromNewtonsoftCtx.embeddedFieldDefault (JsonValueCodecNewtonsoft.jsonValueCodecT ())
            |> toBytes
        
        let dec = efs |> Codec.decoder
        let jsonStr = """{
            "message": "hello"
            }"""
        let decodeResult: Result<(Message*TraceContext), string> = jsonStr |> System.Text.Encoding.Default.GetBytes |> Dec.decode dec
        match decodeResult with
        | Error e -> failwith (sprintf "Parser failed : %s" e)
        | Ok (msg, trace) ->
            printfn "trace=%O" trace
            Assert.True(msg.message = "hello")
            Assert.NotNull(trace.TraceId)
        ()


    [<Fact>]
    let ``Trace header is propagated correctly (FSharp.Data)``() =
        let efs =
            JsonFromFSharpDataCtx.embeddedFieldDefault (JsonValueCodecFSharpData.jsonValueCodecT ())
            |> JsonValueCodecFSharpData.toBytes
        
        let dec = efs |> Codec.decoder
        let jsonStr = sprintf """{
            "message": "hello",
            "~trace": {"tid": "%s", "ts": {}}
            }"""
        let guid = Guid.NewGuid().ToString("N")
        let decodeResult: Result<(Message*TraceContext), string> = guid |> jsonStr |> System.Text.Encoding.Default.GetBytes |> Dec.decode dec
        match decodeResult with
        | Error e -> failwith (sprintf "Parser failed : %s" e)
        | Ok (msg, trace) ->
            printfn "trace=%O" trace
            Assert.True(msg.message = "hello")
            Assert.True(trace.TraceId = guid)
        ()


    [<Fact>]
    let ``Trace header is propagated correctly (Json.NET)``() =
        let efs =
            JsonFromNewtonsoftCtx.embeddedFieldDefault (JsonValueCodecNewtonsoft.jsonValueCodecT ())
            |> JsonValueCodecNewtonsoft.toBytes
        
        let dec = efs |> Codec.decoder
        let jsonStr = sprintf """{
            "message": "hello",
            "~trace": {"tid": "%s", "ts": {}}
            }"""
        let guid = Guid.NewGuid().ToString("N")
        let decodeResult: Result<(Message*TraceContext), string> = guid |> jsonStr |> System.Text.Encoding.Default.GetBytes |> Dec.decode dec
        match decodeResult with
        | Error e -> failwith (sprintf "Parser failed : %s" e)
        | Ok (msg, trace) ->
            printfn "trace=%O" trace
            Assert.True(msg.message = "hello")
            Assert.True(trace.TraceId = guid)
        ()

        
    [<Fact>]
    let ``Trace propagation passes the span id (FSharp.Data)``() =
        let pub = TracePublisher.createDefault "test-service" ignoreLogger telemetryEventWriter
        
        // on service A
        let traceCtx0 = TraceContext.originateNew ()
        let serverSpan0 = Span.start pub id "span-server-A" traceCtx0
        
        // call B from within A
        let clientSpan0,(_,traceCtx1) = Span.startPropagate pub id serverSpan0 "span-client-B" id ()
        
        // encode the trace context into JSON
        let traceCtx1Json = CtxCodecFSharpData.encodeTraceCtx traceCtx1
        
        // decoder for a trace context
        let decodeTraceCtx =
          CtxCodecFSharpData.decodeTraceCtx
          >> Result.map (fun ctx -> (),ctx)
        
        // handle request by A on service B
        let _,serverSpan1 = Span.decodeStart pub id "span-server-B" decodeTraceCtx traceCtx1Json
        
        Assert.True (clientSpan0.TraceId = serverSpan0.TraceId)
        Assert.True (clientSpan0.ParentId.Value = serverSpan0.Id)
        Assert.True (serverSpan1.TraceId = clientSpan0.TraceId)
        Assert.True (serverSpan1.ParentId.Value = clientSpan0.Id)

        
    [<Fact>]
    let ``Trace propagation passes the span id (Json.NET)``() =
        let pub = TracePublisher.createDefault "test-service" ignoreLogger telemetryEventWriter
        
        // on service A
        let traceCtx0 = TraceContext.originateNew ()
        let serverSpan0 = Span.start pub id "span-server-A" traceCtx0
        
        // call B from within A
        let clientSpan0,(_,traceCtx1) = Span.startPropagate pub id serverSpan0 "span-client-B" id ()
        
        // encode the trace context into JSON
        let traceCtx1Json = CtxCodecNewtonsoft.encodeTraceCtx traceCtx1
        
        // decoder for a trace context
        let decodeTraceCtx =
          CtxCodecNewtonsoft.decodeTraceCtx
          >> Result.map (fun ctx -> (),ctx)
        
        // handle request by A on service B
        let _,serverSpan1 = Span.decodeStart pub id "span-server-B" decodeTraceCtx traceCtx1Json
        
        Assert.True (clientSpan0.TraceId = serverSpan0.TraceId)
        Assert.True (clientSpan0.ParentId.Value = serverSpan0.Id)
        Assert.True (serverSpan1.TraceId = clientSpan0.TraceId)
        Assert.True (serverSpan1.ParentId.Value = clientSpan0.Id)

    [<Fact>]
    let ``Completed span should have a duration``() =
        let pub = TracePublisher.createDefault "test-service" ignoreLogger telemetryEventWriter
        let traceCtx = TraceContext.originateNew ()
        let span = Span.start pub id "span-server-A" traceCtx
        System.Threading.Thread.Sleep 100
        let span' = Span.complete pub span
        Assert.True (span.Duration = None)
        Assert.True (span'.Duration <> None)

    
    [<Fact>]
    let ``Can't complete an already completed span``() =
        let pub = TracePublisher.createDefault "test-service" ignoreLogger telemetryEventWriter
        let traceCtx = TraceContext.originateNew ()
        let span = Span.start pub id "span-server-A" traceCtx
        System.Threading.Thread.Sleep 100
        let span = Span.complete pub span
        Assert.Throws<exn>((fun () -> Span.complete pub span |> ignore))

    
    [<Fact>]
    let ``A span is a span/cospan of events``() =
    
        let pub = TracePublisher.createDefault "test-service" ignoreLogger telemetryEventWriter
        let traceCtx = TraceContext.originateNew ()
        
        let span = Span.start pub id "span-server-A" traceCtx
        let startedEvent = span |> Span.toTelemetryEvent EVENT_TYPE_STARTED
        let span' = startedEvent |> Span.ofTelemetryEvent
        Assert.True ((span = span'), "startedSpan")
        
        let completedSpan = Span.complete pub span
        let completedEvent = completedSpan |> Span.toTelemetryEvent EVENT_TYPE_COMPLETED
        let completedSpan' = completedEvent |> Span.ofTelemetryEvent
        Assert.True ((completedSpan = completedSpan'), "completedSpan")
        Assert.True ((completedSpan.Id = completedSpan'.Id), "completedSpan")
        Assert.True ((completedSpan.Name = completedSpan'.Name), "completedSpan")
        Assert.True ((completedSpan.TraceId = completedSpan'.TraceId), "completedSpan")
        Assert.True ((completedSpan.Duration = completedSpan'.Duration), "completedSpan")
        Assert.True ((completedSpan.ParentId = completedSpan'.ParentId), "completedSpan")
        Assert.True ((completedSpan.Tags = completedSpan'.Tags), "completedSpan")

    [<Fact>]
    let ``Telemetry event is properly escaped``() =
        let te = {
            TraceId = "42"
            EventType = ""
            Timestamp = DateTimeOffset.MinValue
            Tags = TraceTags.ofSeq [ "e\"scape","the" ; "special", "chars" ] }
        let sb = System.Text.StringBuilder ()
        telemetryEventWriter te sb
        let str = string sb
        Assert.True ((str = """{"ts":"0001-01-01T00:00:00.000+00:00","et":"","tid":"42","tags":{"e\"scape":"the","special":"chars"}}"""))