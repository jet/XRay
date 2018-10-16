namespace XRay


/// <summary>Represents a communication channel.</summary>
/// <note>
/// Broadly, a channel interaction is that of a server or a client.
/// A server channel can be used to create spans representing an operation performed by a service in handling a request.
/// A client channel can be used to create spans representing a server invoking a client, and thereby relating the server and client spans.
/// An interaction with a channel involves transmitting messages across the channel as well as extracting and embedding trace data into
/// those messages.
/// </note>
type Channel = {
    /// The logger.
    Publisher: TracePublisher

    /// The name of the channel.
    Name: string

    /// Tags applied to telemetry events created in the context of this channel.
    Tags: TraceTags }

    with
        member internal __.apply tt =
            tt
            |> TraceTags.merge __.Tags
            |> TraceTags.addChannel __.Name

/// A request-reply server channel wherein the channel
/// handles inputs of type 'i and produces outputs of type 'o.
/// The server is expressed in terms of inputs of type 'a
/// and outputs of type 'b.
type ReqRepServer<'i, 'o, 'a, 'b> = { 
    Ch: Channel
    Decoder: Dec<'i, 'a * TraceContext>
    Encoder: Enc<'o, 'b * TraceContext> }

/// A request-reply client channel wherein the channel
/// transmits inputs of type 'i and responds with outputs of type 'o.
/// The consumer of the client is expressed in terms of domain-specific
/// input type 'a and output type 'b.
type ReqRepClient<'i, 'o, 'a, 'b> = private {
    Ch: Channel
    Encoder: Enc<'i, 'a * TraceContext>
    Decoder: Dec<'o, 'b * TraceContext> }

/// A publish-subscribe channel with inputs of type 'i, outputs of type 'o
/// and domain-specific message type 'a.
type PubClient<'m, 'a> = { 
    Ch: Channel
    Encoder: Enc<'m, 'a * TraceContext> }

/// A publish-subscribe channel with inputs of type 'i, outputs of type 'o
/// and domain-specific message type 'a.
type SubClient<'m, 'a> = { 
    Ch: Channel
    Decoder: Dec<'m, 'a * TraceContext> }


/// Operations on channels.
[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Channel =

    /// Starts a server operation within a trace.
    let startServer (ch: Channel) spanName (trace: TraceContext) : Span = Span.start ch.Publisher ch.apply spanName trace
    
    /// Completes a server operation.
    let completeServer (ch: Channel) span : Span = Span.complete ch.Publisher span
    
    /// Error a server operation.
    let errorServer (ch: Channel) span (error: string) : Span = Span.error ch.Publisher error span 
    
    /// Starts a server operation.
    /// Decodes a channel input into a domain-specific input.
    /// Throws an exception if decoding fails.
    let startServerDecode (ch: Channel) spanName (dec: Dec<'i, 'a * TraceContext>) (i: 'i) : 'a * Span = Span.decodeStart ch.Publisher ch.apply spanName dec i
    
    /// Completes a server operation.
    let completeServerEncode (ch: Channel) span (enc: Enc<'o, 'b * TraceContext>) (b: 'b) : 'o = Span.completeEcho ch.Publisher span enc b
    
    let serverCodec
        (ch: Channel)
        (spanName: string)
        (dec: Dec<'i, 'a * TraceContext>)
        (enc: Enc<'o, 'b * TraceContext>)
        (f: Span -> 'a -> Async<'b>) : 'i -> Async<'o> =
            Span.server ch.Publisher ch.apply spanName dec enc f
    
    /// Starts a client operation.
    let startClient (ch: Channel) clientSpanName (op: Span) : Span = Span.startClient ch.Publisher ch.apply clientSpanName op
    
    /// Completes a client operation.
    let completeClient (ch: Channel) span = Span.complete ch.Publisher span
    
    /// Errors a client operation.
    let errorClient (ch: Channel) span (error: string) : Span = Span.error ch.Publisher error span
    
    /// Starts a client invocation operation.
    /// Encodes a domain-specific input 'a along with the trace into a channel-specific input 'i.
    let startClientEncode (ch: Channel) clientSpanName span (enc: Enc<'i, 'a * TraceContext>) (a: 'a) : Span * 'i = Span.startPropagate ch.Publisher ch.apply span clientSpanName enc a
    
    /// Completes an invocation of a client operation.
    /// Decodes a channel output into a domain-specific output.
    /// Throws an exception if decoding fails.
    let completeClientDecode (ch: Channel) span (dec: Dec<'o, 'b * TraceContext>) (o: 'o) : 'b * TraceContext = Span.completeAck ch.Publisher span dec o
    
    let client
        (ch: Channel)
        (clientSpanName: string)
        (enc: Enc<'i, 'a * TraceContext>)
        (dec: Dec<'o, 'b * TraceContext>)
        (f: 'i -> Async<'o>) : Span -> 'a -> Async<'b> =
            Span.client ch.Publisher ch.apply clientSpanName enc dec f


open System.Net.Http
type HTTP (ctxCodec) =
    let httpCodec = Codec.Http ctxCodec
    member __.server (pub:TracePublisher) : ReqRepServer<HttpRequestMessage, HttpResponseMessage, HttpRequestMessage, HttpResponseMessage> =
        {
            ReqRepServer.Ch = { Channel.Publisher = pub ; Name = "http" ; Tags = [ TAG_ROLE,TAG_VALUE_ROLE_SERVER ] |> TraceTags.ofSeq }
            Decoder = httpCodec.Req |> Codec.decoder
            Encoder = httpCodec.Res |> Codec.encoder
        }

    member __.client (pub:TracePublisher) : ReqRepClient<HttpRequestMessage, HttpResponseMessage, HttpRequestMessage, HttpResponseMessage> =
        {
            ReqRepClient.Ch = { Channel.Publisher = pub ; Name = "http" ; Tags = [ TAG_ROLE,TAG_VALUE_ROLE_CLIENT ] |> TraceTags.ofSeq }
            Decoder = httpCodec.Res |> Codec.decoder
            Encoder = httpCodec.Req |> Codec.encoder
        }


  /// Kafka channels.
  module Kafka =
      let pub log = { Channel.Name = "kafka"; Tags = TraceTags.empty; Publisher = log }
      let sub log = { Channel.Name = "kafka"; Tags = TraceTags.empty; Publisher = log }


/// Operations on request reply servers.
[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module ReqRepServer =

    /// Starts an operation to handle a request on a server.
    /// A trace is extracted from the specified request.
    let start (ch: ReqRepServer<'i, 'o, 'a, 'b>) spanName (i: 'i) : 'a * Span = Channel.startServerDecode ch.Ch spanName ch.Decoder i
      
    /// Completes an operation to handle a request on a server.
    let complete (ch: ReqRepServer<'i, 'o, 'a, 'b>) span (b: 'b) : 'o = Channel.completeServerEncode ch.Ch span ch.Encoder b
    
    let error (ch: ReqRepServer<'i, 'o, 'a, 'b>) span (error: string) : Span = Channel.errorServer ch.Ch span error
    
    /// Creates a server for a request-reply channel.
    let server (ch: ReqRepServer<'i, 'o, 'a, 'b>) spanName (f: Span -> 'a -> Async<'b>) : 'i -> Async<'o> = Channel.serverCodec ch.Ch spanName ch.Decoder ch.Encoder f


/// Operations on request-reply channel clients.
[<RequireQualifiedAccessAttribute>]
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module ReqRepClient =

    /// Starts a client invocation operation.
    let start (ch: ReqRepClient<'i, 'o, 'a, 'b>) clientOpName (op: Span) (a: 'a) : Span * 'i = Channel.startClientEncode ch.Ch clientOpName op ch.Encoder a
    
    /// Completes an invocation of a client operation.
    let complete (ch: ReqRepClient<'i, 'o, 'a, 'b>) span (o: 'o) : 'b * TraceContext = Channel.completeClientDecode ch.Ch span ch.Decoder o
      
    let client (ch: ReqRepClient<'i, 'o, 'a, 'b>) clientOpName (f: 'i -> Async<'o>) : Span -> 'a -> Async<'b> = Channel.client ch.Ch clientOpName ch.Encoder ch.Decoder f


/// Operations on publish-subscribe channels.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PubClient =

    /// Starts a client operation to produce a message in the context of the specified operation.
    /// The trace is injected into the specified client request using a carrier.
    let start (ch: PubClient<'m, 'a>) (op: Span) clientOpName (a: 'a) : Span * 'm = Channel.startClientEncode ch.Ch clientOpName op ch.Encoder a

    /// Completes a client operation to produce a message.
    /// A telemetry event is emitted indicating the completion of the client operation.
    let complete (ch: PubClient<'m, 'a>) (cop: Span) : Span = Channel.completeClient ch.Ch cop
      
    /// Indicates that a client operation errored.
    let error (ch: PubClient<'m, 'a>) (cop: Span) (error: string) : Span = Channel.errorClient ch.Ch cop error
    
    let producer (ch: PubClient<'m, 'a>) clientOpName (f: 'm -> Async<'r>) : Span -> 'a -> Async<'r> = Channel.client ch.Ch clientOpName ch.Encoder (Dec.create (fun r -> Ok (r, TraceContext.originateNew ()))) f


/// Operations on publish-subscribe channels.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SubClient =

    /// Starts an operation to consume a message from a publish-subscribe channel.
    let start (ch: SubClient<'m, 'a>) opName (m: 'm) : 'a * Span = Channel.startServerDecode ch.Ch opName ch.Decoder m
    
    /// Completes an operation consuming a message from a publish-subscribe channel.
    let complete (ch: SubClient<'m, 'a>) (op: Span) : Span = Channel.completeServer ch.Ch op
      
    let error (ch: SubClient<'m, 'a>) (op: Span) (error: string) : Span = Channel.errorServer ch.Ch op error
    
    let consumer (ch: SubClient<'m, 'a>) opName (f: Span -> 'a -> Async<'b>) : 'm -> Async<'b> = Channel.serverCodec ch.Ch opName ch.Decoder (fun (b, _) -> b) f