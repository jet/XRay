namespace XRay

open System

/// Operations on ExceptionDispatchInfo.
[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module ExceptionDispatchInfo =

    open System.Runtime.ExceptionServices
    
    /// Throws the captured exception using ExceptionDispatchInfo.Throw.
    let inline throw (edi: ExceptionDispatchInfo) =
        edi.Throw ()
        failwith "unreachable"
    
    /// Captures and re-throws an exception using ExceptionDispatchInfo.
    let inline captureThrow (ex: exn) =
        let edi = ExceptionDispatchInfo.Capture ex in
        throw edi

/// <summary>A span.</summary>
/// <remarks>
/// A span corresponds to an operation performed by a server or a client.
/// In performing an operation, a server starts a span. Outgoing communications are represented
/// as client spans, nested within the ambient server span. A client span has a corresponding server span on
/// the destination server.
/// Spans are delimited by start and completion telemetry events, and may also include intermediate telemetry events.
/// </remarks>
type Span = {

    /// The trace which the span is a part of.
    TraceId: string
    
    /// Unique id for this span.
    Id: string
    
    /// Id of the parent span, if any.
    ParentId: string option
    
    /// The name of the span.
    Name: string
    
    /// The start time of the span.
    Started: DateTimeOffset
    
    /// The duration of the span.
    Duration: TimeSpan option
    
    /// Tags (such as the duration, if the span has completed, parent span, and other user-defined tags).
    Tags: TraceTags }

/// Operations on spans.
[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =

    let private guid () = Guid.NewGuid().ToString "N"
    
    /// Determines whether the event type represents a completion (or error) of a span.
    let private isCompletedEventType (eventType: string) =
        match eventType with
        | EVENT_TYPE_COMPLETED | EVENT_TYPE_ERROR -> true
        | _ -> false
    
    /// Determines whether the span is completed.
    let private isCompletedSpan (s: Span) = s.Duration.IsSome

    let private ensureNotCompleted (s: Span) = if isCompletedSpan s then failwithf "span=%s trace_id=%s has already completed" s.Name s.TraceId

    let private isValidEventType (eventType: string) =
        match eventType with
        | EVENT_TYPE_STARTED | EVENT_TYPE_COMPLETED | EVENT_TYPE_ERROR | EVENT_TYPE_INFO -> true
        | _ -> false

    /// <summary>Creates a telemetry event of the specified type for the Span.</summary>  
    /// <exception cref="System.Exception">Thrown when the specified event type is not valid.</exception>
    /// <remarks>For example: an event indicating that the span has started or completed.</remarks>
    let toTelemetryEvent (eventType: string) (s: Span) : TelemetryEvent =
        if not (isValidEventType eventType) then failwithf "event_type=%s is not a valid event type" eventType
        if isCompletedEventType eventType && not (isCompletedSpan s) then failwithf "attemted to create completion event_type=%s for incomplete span=%s trace_id=%s" eventType s.Name s.TraceId
        let now =
            match eventType with
            | EVENT_TYPE_STARTED -> s.Started
            | _ -> now ()
        let tags =
            s.Tags
            |> TraceTags.addString TAG_SPAN s.Name
            |> TraceTags.addString TAG_SPAN_ID s.Id
            |> TraceTags.addIf TAG_PARENT_SPAN_ID s.ParentId
            |> TraceTags.addTimeSpanIf TAG_OP_DURATION s.Duration
        TelemetryEvent.create now s.TraceId eventType tags

    /// <summary>Extracts a span from a telemetry event.</summary>
    /// <returns>A span associated with the telemetry event or None if unable to decode.</returns>
    /// <remarks>
    /// A span is delimited by two telemetry events - a started and completed event.
    /// Conversely, a telemetry event (typically) corresponds to a span, indicating whether
    /// a span has started or completed.
    /// </remarks>  
    let tryOfTelemetryEvent (e: TelemetryEvent) : Span option =
        let traceId = e.TraceId
        let spanName = e.Tags |> TraceTags.tryFind TAG_SPAN
        let spanId = e.Tags |> TraceTags.tryFind TAG_SPAN_ID
        let parentSpanId = e.Tags |> TraceTags.tryFind TAG_PARENT_SPAN_ID
        let duration = e.Tags |> TraceTags.tryFindTimeSpan TAG_OP_DURATION
        let started =
            match e.EventType with
            | EVENT_TYPE_COMPLETED | EVENT_TYPE_ERROR -> e.Timestamp - (match duration with Some d -> d | None -> TimeSpan.Zero)
            | _ -> e.Timestamp
        let tags = e.Tags |> TraceTags.exceptTags (set [TAG_SPAN;TAG_SPAN_ID;TAG_PARENT_SPAN_ID;TAG_OP_DURATION])
        match spanName, spanId with
        | Some name, Some spanId -> Some { Span.Name = name; TraceId = traceId; Started = started; Tags = tags; Id = spanId; ParentId = parentSpanId; Duration = duration }
        | _ -> None
    
    /// <summary>Extracts a span from a telemetry event.</summary>
    /// <exception cref="System.Exception">Thrown when the telemetry event doesn't correspond to a span.</exception>
    let ofTelemetryEvent (e: TelemetryEvent) : Span =
        match tryOfTelemetryEvent e with
        | Some s -> s
        | None -> failwithf "missing span_name or span_id in trace_id=%s" e.TraceId
    
    /// <summary>Creates a trace context (signature) to embed into a message to be propagated, indicating that
    /// it is in the scope of the specified span.
    /// </summary>
    /// <remarks>
    /// This operation is used to propagate a trace across service boundaries.
    /// Receivers of the trace context will mark the receiving span as a child of the span specified herein.
    /// </remarks>
    let toTraceContext (s: Span) =
        // TODO: consider propagating additional tags?
        let tags =
            TraceTags.empty
            |> TraceTags.addString TAG_SPAN_ID s.Id
        TraceContext.create s.TraceId tags
    
    /// Creates a (server) span within the specified trace.
    /// Marks the span identified in the trace context, if any, as the parent of the created span.   
    let create (tags:TraceTags -> TraceTags) (name:string) (trace:TraceContext) : Span =
        let spanId = guid ()
        let tags = TraceTags.empty |> tags
        let parentId = TraceTags.tryFind TAG_SPAN_ID trace.Tags
        { Span.Name = name ; TraceId = trace.TraceId ; Started = now () ;
          Tags = tags ; Id = spanId ; ParentId = parentId ; Duration = None }

    /// Starts a (server) span within the specified trace.
    /// Marks the span identified in the trace context, if any, as the parent of the created span.
    let start (pub:TracePublisher) (tags:TraceTags -> TraceTags) (name:string) (trace:TraceContext) : Span =
        let span = create tags name trace
        TracePublisher.publish pub <| toTelemetryEvent EVENT_TYPE_STARTED span 
        span

    /// Creates a client span within the specified (server) span.
    /// Sets the specified span as the parent of the created (client) span.
    let createClient (tags:TraceTags -> TraceTags) (clientSpanName:string) (span:Span) : Span =
        let spanId = guid ()
        let tags = TraceTags.empty |> tags
        { Span.Name = clientSpanName ; TraceId = span.TraceId ; Started = now () ;
          Tags = tags ; Id = spanId ; ParentId = Some span.Id ; Duration = None }

    /// Starts a client span within the specified (server) span.
    /// Sets the specified span as the parent of the created (client) span.
    let startClient (pub:TracePublisher) (tags:TraceTags -> TraceTags) (clientSpanName:string) (span:Span) : Span =
        let span = createClient tags clientSpanName span
        TracePublisher.publish pub <| toTelemetryEvent EVENT_TYPE_STARTED span 
        span

    /// Normalizes a duration to have a integral millisecond resolution
    let private normalizeDurationResolution (d: TimeSpan) = TimeSpan.FromMilliseconds (float (int64 d.TotalMilliseconds))
    
    /// Completes a span.
    let complete (pub: TracePublisher) span : Span =
        ensureNotCompleted span
        let duration = normalizeDurationResolution (now () - span.Started)
        let span = { span with Duration = Some duration }
        TracePublisher.publish pub <| toTelemetryEvent EVENT_TYPE_COMPLETED span 
        span
    
    /// Errors (and completes) a span.
    let error (pub: TracePublisher) (error: string) span : Span =
        ensureNotCompleted span
        let duration = normalizeDurationResolution (now () - span.Started)
        let tags =
            span.Tags
            |> TraceTags.addMessage error
        let span = { span with Tags = tags; Duration = Some duration }
        TracePublisher.publish pub <| toTelemetryEvent EVENT_TYPE_ERROR span 
        span
    
    // ------------------------------------------------------------------------------------------------------
    // workflows
    
    /// Starts a server span.
    /// Decodes a channel input into a domain-specific input.
    /// Returns the domain-specific input and the span.
    /// Throws an exception if decoding fails.
    let decodeStart (pub: TracePublisher) (tags: TraceTags -> TraceTags) spanName (dec: Dec<'i, 'a * TraceContext>) : 'i -> 'a * Span =
        fun i ->
            match Dec.decode dec i with
            | Ok (a, ctx) ->
                let op = start pub tags spanName ctx
                a, op
            | Error e -> failwithf "error decoding input for span=%s : %s" spanName e
    
    /// Completes a server span.
    /// Encodes a domain-specific input and the trace into an output value.
    let completeEcho (pub: TracePublisher) span (enc: Enc<'o, 'b * TraceContext>) (b: 'b) : 'o =
        let span = complete pub span
        let ctx = toTraceContext span
        (b, ctx) |> Enc.encode enc
    
    /// Creates a server accepting inputs of type 'i and producing output of type 'o
    /// based on a function accepting the ambient span of the given name and a domain-specific input of type 'a
    /// producing a domain-specific output of type 'b.
    /// If the provided function throws, the exception is escalated and the span is completed in error.
    /// Telemetry events are emitted to the specified log.
    let server
        (pub: TracePublisher)
        (tags: TraceTags -> TraceTags)
        (spanName: string)
        (dec: Dec<'i, 'a * TraceContext>)
        (enc: Enc<'o, 'b * TraceContext>)
        (f: Span -> 'a -> Async<'b>) : 'i -> Async<'o> =
        fun i -> async {
            let (a, span) = decodeStart pub tags spanName dec i
            try
                let! b = f span a
                return completeEcho pub span enc b
            with ex ->
                let span' = error pub ex.Message span in
                return ExceptionDispatchInfo.captureThrow ex }
    
    /// Starts a client span in the context of a server span.
    /// Encodes a domain-specific input into a channel input, propagating the trace.
    /// Returns a span representing the client invocation and the input containing the trace context to transmit using the client.
    let startPropagate
        (pub: TracePublisher)
        (tags: TraceTags -> TraceTags)
        (span: Span)
        (clientSpanName: string)
        (enc: 'a * TraceContext -> 'i) : 'a -> Span * 'i =
        fun a ->
            let clientSpan = startClient pub tags clientSpanName span
            let ctx = toTraceContext clientSpan
            let i = (a, ctx) |> Enc.encode enc
            clientSpan, i
    
    /// Completes a client span.
    /// Decodes a channel output into a domain-specific output.
    /// Throws an exception if decoding fails.
    let completeAck (pub: TracePublisher) clientSpan (dec: Dec<'o, 'b * TraceContext>) (o: 'o) : 'b * TraceContext =    
        match Dec.decode dec o with
        | Ok (b, ctx) ->
            let _span = complete pub clientSpan
            b, ctx
        | Error e -> failwithf "error decoding output for client span=%s trace_id=%s : %s" clientSpan.Name clientSpan.TraceId e
    
    /// Creates a client in the contex of an ambient server span, accepting domain-specific inputs of type 'a and returning domain-specific
    /// outputs of type 'b. The client span is nested within the ambient span.
    let client
        (pub: TracePublisher)
        (tags: TraceTags -> TraceTags)
        (clientOpName: string)
        (enc: Enc<'i, 'a * TraceContext>)
        (dec: Dec<'o, 'b * TraceContext>)
        (f: 'i -> Async<'o>) : Span -> 'a -> Async<'b> =
        fun span (a: 'a) -> async {
            let span', i = startPropagate pub tags span clientOpName enc a
            try
                let! o = f i
                let (b, _trace) = completeAck pub span' dec o
                return b
            with ex ->
                let _span = error pub ex.Message span
                return ExceptionDispatchInfo.captureThrow ex }