namespace XRay

[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("XRay.Tests")>]
do()

open System


/// Encodes a value of type 'a into a value of type 'o.
type Enc<'o, 'a> = 'a -> 'o

/// Decodes a value of type 'i into a value of type 'a, possibly returning None.
type Dec<'i, 'a> = 'i -> Result<'a, string>

/// A decoder from type 'i and encoder to type 'o for types 'a and 'b.
type Codec<'i, 'o, 'a, 'b> = Dec<'i, 'a> * Enc<'o, 'b>

/// A decoder from type 'i and encoder to type 'o for type 'a.
type Codec<'i, 'o, 'a> = Codec<'i, 'o, 'a, 'a>

/// A codec for raw type 'm to type 'a.
type Codec<'m, 'a> = Codec<'m, 'm, 'a>


/// Operations on encoders.
[<RequireQualifiedAccessAttribute>]
module Enc =

    /// Creates an encoder.
    let inline create (f: 'a -> 'o) : Enc<'o, 'a> = f

    /// Encodes a value.
    let inline encode (e: Enc<'o, 'a>) (a: 'a) : 'o = e a


/// Operations on decoders.
[<RequireQualifiedAccessAttribute>]
module Dec =

    /// Creates an decoder.
    let inline create (f: 'i -> Result<'a, string>) : Dec<'i, 'a> = f

    /// Decodes a value.
    let inline decode (d: Dec<'i, 'a>) (i: 'i) : Result<'a, string> = d i



[<AutoOpen>]
module Json =
    open System.Globalization
    
    type Int64 with
        static member parse x =
            let outToOption (has, value) = if has then Some value else None
            Int64.TryParse (x, NumberStyles.Any, CultureInfo.InvariantCulture) |> outToOption
    
    
    /// Merges two maps favoring values from the second.
    let mergeMap (a: Map<'a, 'b>) (b: Map<'a, 'b>) =
        let keys m = m |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        Set.union (keys a) (keys b)
        |> Seq.choose (fun k ->
            match Map.tryFind k a, Map.tryFind k b with
            | _, Some b -> Some (k, b)
            | Some a, _ -> Some (k, a)
            | _ -> None )
        |> Map.ofSeq
    
    
[<AutoOpen>]
module internal Time =

    /// Returns the current time.
    let inline now () =
#if NO_UNIXTIMEMILLISECONDS
        DateTimeOffset.UtcNow.ToFileTime()
        |> fun t -> t - (t % 10000L)
        |> DateTimeOffset.FromFileTime
        |> fun d -> d.ToUniversalTime()
#else
        DateTimeOffset.UtcNow.ToUnixTimeMilliseconds ()
        |> DateTimeOffset.FromUnixTimeMilliseconds
#endif


/// A collection of trace tags.
type TraceTags = private { Tags: Map<string, string>}
    


/// A telemetry event - this is the unit of indexing in XRay.
type TelemetryEvent = {

    /// The trace id.
    TraceId : string
    
    /// The event type.
    EventType : string
    
    /// The timestamp.
    Timestamp : DateTimeOffset
    
    /// The tags.
    Tags : TraceTags } 



/// Conventions for tags keys and values.
[<AutoOpen>]
module Literals =

    /// The event type indicating an operation started.
    let [<Literal>] EVENT_TYPE_STARTED = "started"

    /// The event type indicating that an operation completed.
    let [<Literal>] EVENT_TYPE_COMPLETED = "completed"

    /// The event type indicating an INFO message.
    let [<Literal>] EVENT_TYPE_INFO = "info"

    /// The event type indicating an ERROR message.
    let [<Literal>] EVENT_TYPE_ERROR = "error"


    /// This tag contain the service name.
    let [<Literal>] TAG_SERVICE_NAME = "serviceName"

    /// The span id (guid).
    let [<Literal>] TAG_SPAN_ID = "sid"

    /// The parent span id (guid).
    let [<Literal>] TAG_PARENT_SPAN_ID = "psid"

    /// The duration of an operation.
    let [<Literal>] TAG_OP_DURATION = "duration"

    /// This tag contains the operation/span name.  
    let [<Literal>] TAG_SPAN = "op"

    /// The name of the client operation.
    let [<Literal>] TAG_CLIENT_OP = "cop"

    /// The role of the service emitting the event with respect to
    /// the message handled ("(c)lient"|"(s)erver"|"(p)roducer"|"co(n)sumer")
    let [<Literal>] TAG_ROLE = "r"

    let [<Literal>] TAG_VALUE_ROLE_CLIENT = "c"
    let [<Literal>] TAG_VALUE_ROLE_SERVER = "s"

    let [<Literal>] TAG_VALUE_ROLE_PRODUCER = "p"
    let [<Literal>] TAG_VALUE_ROLE_CONSUMER = "r"

    /// A generic message tag.
    let [<Literal>] TAG_MESSAGE = "msg"

    /// The channel ("http"|"kafka"|"azureq"|"eventstore")
    let [<Literal>] TAG_CHANNEL = "ch"

    /// HTTP channel.
    let [<Literal>] TAG_VALUE_CHANNEL_HTTP = "http"

    /// Kafka channel.
    let [<Literal>] TAG_VALUE_CHANNEL_KAFKA = "kafka"

    /// EventStore channel.
    let [<Literal>] TAG_VALUE_CHANNEL_EVENTSTORE = "eventstore"

    /// AzureQueues channel.
    let [<Literal>] TAG_VALUE_CHANNEL_AZUREQUEUE = "azureq"

    /// Azure Service Bus channel
    let [<Literal>] TAG_VALUE_CHANNEL_AZUREBUS = "azuresb"


/// Operations on trace tag collections.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceTags =

    let private un (t: TraceTags) = t.Tags
    
    /// Empty tags.
    let empty : TraceTags = { Tags = Map.empty }
    
    /// Creates trace tags from a map.
    let ofMap (m: Map<string, string>) : TraceTags = { Tags = m }
    
    /// Creates trace tags from a sequence.
    let ofSeq (m: (string * string) seq) : TraceTags = ofMap (Map.ofSeq m)
    
    /// Converts trace tags to a sequence.
    let toSeq (m: TraceTags) : seq<string * string> = m |> un |> Map.toSeq
    
    /// Converts trace tags to a map.
    let toMap (m: TraceTags) : Map<string, string> = m |> un
    
    let private update (f: Map<string, string> -> Map<string, string>) (t: TraceTags) : TraceTags = t |> un |> f |> ofMap
    
    /// Adds a tag.
    let addString (k: string) (v: string) (m: TraceTags) : TraceTags = update (Map.add k v) m
    
    /// Adds an int32 tag.
    let addInt32 (k: string) (v: int32) (m: TraceTags) : TraceTags = addString k (string v) m
    
    /// Adds an int64 tag.
    let addInt64 (k: string) (v: int64) (m: TraceTags) : TraceTags = addString k (string v) m
    
    /// Adds a timespan tag, encoded as a string containing an int64 milliseconds.
    let addTimeSpan (k: string) (v: TimeSpan) (m: TraceTags) : TraceTags = addInt64 k (int64 v.TotalMilliseconds) m
    
    /// Adds a bool tag.
    let addBool (k: string) (v: bool) (m: TraceTags) : TraceTags = addString k (string v) m
    
    /// Adds a tag if some.
    let addIf (k: string) (v: string option) (m: TraceTags) : TraceTags =
        match v with
        | Some v -> m |> un |> Map.add k v |> ofMap
        | None -> m
    
    /// Adds a tag if not equal to TimeSpan.Zero.
    let addTimeSpanIf (k: string) (v: TimeSpan option) (m: TraceTags) : TraceTags =
        match v with
        | Some v -> addTimeSpan k v m
        | None -> m
    
    /// Adds a collection of tags.
    let addMany (kvps:(string * string) seq) (m: TraceTags) : TraceTags = (un m, kvps) ||> Seq.fold (fun m (k, v) -> Map.add k v m) |> ofMap
    
    /// Concatenates two sets of trace tags.
    let merge (t1: TraceTags) (t2: TraceTags) : TraceTags = mergeMap (un t1) (un t2) |> ofMap
    
    /// Tries to find a tag.
    let tryFind (key: string) (t: TraceTags) : string option = t.Tags |> Map.tryFind key
    
    /// Tries to find a tag.
    let tryFindTimeSpan (key: string) (t: TraceTags) : TimeSpan option = tryFind key t |> Option.bind (Int64.parse >> Option.map (float >> TimeSpan.FromMilliseconds))
    
    /// Filters the set of tags to those having the specified prefix.
    let filterByTagPrefix (prefix: string) : TraceTags -> TraceTags = update (Map.filter (fun k _ -> k.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
    
    /// Filters the set of tags to those of the specified set of tag names.
    let filterByTags (tags: Set<string>) : TraceTags -> TraceTags = update (Map.filter (fun k _ -> Set.contains k tags))
    
    /// Filters the set of tags to those not in the specified set of tag names.
    let exceptTags (tags: Set<string>) : TraceTags -> TraceTags = update (Map.filter (fun k _ -> not (Set.contains k tags)))
    
    
    // ----------------------------------------------------------------------------------------
    // conventions
    
    /// Adds an operation name tag.
    let addOpName (opName: string) (t: TraceTags) = addString TAG_SPAN opName t
    
    /// Adds a message tag.
    let addMessage (msg: string) (t: TraceTags) = addString TAG_MESSAGE msg t
    
    /// Adds a role tag, indicating the role of the span with respect to the communication channel.
    let addRole (role: string) =
        if String.IsNullOrEmpty role then invalidArg "role" "role name must be specified"
        addString TAG_ROLE role
    
    let roleProducer = addRole TAG_VALUE_ROLE_PRODUCER
    
    let roleConsumer = addRole TAG_VALUE_ROLE_CONSUMER
    
    /// Adds a channel tag.
    let addChannel (ch: string) =
        if String.IsNullOrEmpty ch then invalidArg "ch" "channel name must be specified"
        addString TAG_CHANNEL ch
    
    /// Adds a tag denoting the duration of an operation.
    let addOpDuration (elapsed: TimeSpan) = addTimeSpan TAG_OP_DURATION elapsed
    
    let tryFindSpanId = tryFind TAG_SPAN_ID
    
    let tryFindParentSpanId = tryFind TAG_PARENT_SPAN_ID


/// <summary>A trace context is propagated via messages across service boundaries.</summary>
/// <remarks>
/// The most essential part of the trace (span) context is the trace id, propagation of which allows
/// telemetry events emitted by separate services to be correlated. In addition, this context carries
/// information about the span which was used to propagated, allowing causality relationships between
/// spans to be established.
/// </remarks>
type TraceContext = {

    /// The trace id.
    TraceId: string

    /// Trace tags.
    Tags: TraceTags }
    

/// Operations on trace contexts.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceContext =

    let private newTraceId () = Guid.NewGuid().ToString "N"

    /// Creates a new trace context.
    let create (traceId: string) (tags: TraceTags) : TraceContext =
        if String.IsNullOrEmpty traceId then invalidArg "traceId" "trace_id must be non-empty"
        { TraceId = traceId; Tags = tags }

    /// Creates a new trace context.
    let originate (traceId: string) : TraceContext = create traceId TraceTags.empty

    /// Creates a new trace context using a new Guid for the trace_id.
    let originateNew () : TraceContext = originate (newTraceId ())


/// Constructors for telemetry events.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module TelemetryEvent =
 
    /// Creates a telemetry event.
    let create (timestamp: DateTimeOffset) (traceId: string) (eventType: string) (tags: TraceTags) : TelemetryEvent = {
        TraceId = traceId
        EventType = eventType
        Timestamp = timestamp
        Tags = tags }