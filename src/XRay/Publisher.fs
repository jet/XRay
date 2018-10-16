namespace XRay

open System.Collections.Concurrent
open System.Threading


/// An asynchronous implementation of IEvent<_>.
/// Triggers are placed into a buffer, and published on a separate thread.
type internal AsyncEvent<'a> (bufferSize: int) =
    let mutable st = 0
    let buf = new BlockingCollection<'a> (bufferSize)
    let evt = new Event<'a>()
    let trigger () =
        for a in buf.GetConsumingEnumerable () do
            evt.Trigger a
    do (let t = new Thread (ThreadStart (trigger)) in t.IsBackground <- true; t.Start ())
    /// Puts an event into a buffer to be published asyncrhonously.
    member __.Trigger (a: 'a) =
        if st = 0 then
            // try add can throw exs
            // todo: think about condition where
            // triggered but stopped
            buf.TryAdd a |> ignore
    /// Publishes the observation.
    member __.Publish = evt.Publish
    /// Stops the event and the publishing thread.
    member __.Stop () = if Interlocked.CompareExchange (&st, 1, 0) = 0 then buf.CompleteAdding ()

type TracePublisherConfig = {
    /// Tags added to all logged telemetry events.
    Tags: TraceTags
    Filter: TelemetryEvent -> TelemetryEvent option
    EventBatchSize: int }
    with
        static member internal Default = { Tags = TraceTags.empty; Filter = Some; EventBatchSize = 1000000 }

type TracePublisher = internal {
    config: TracePublisherConfig
    publisher: AsyncEvent<TelemetryEvent> }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module TracePublisher =
    open System.Text

    let create config listener sbWriter =
        let pub = {
            config = config
            publisher = new AsyncEvent<_> (config.EventBatchSize) }
        let builder = new StringBuilder ()
        pub.publisher.Publish.Subscribe (fun e ->
            match config.Filter e with
            | Some e ->
                do sbWriter e builder
                listener (string builder)
                do builder.Clear() |> ignore
            | None -> ()) |> ignore
        pub

    let createDefault (serviceName:string) listener sbWriter =
        let config = TracePublisherConfig.Default
        create { config with Tags = config.Tags |> TraceTags.addString TAG_SERVICE_NAME serviceName } listener sbWriter

    /// Logs a telemetry event.
    let publish (pub: TracePublisher) (event:TelemetryEvent) : unit =
        // TODO: optimize to avoid mutation
        let e = { event with Tags = TraceTags.merge event.Tags pub.config.Tags }    
        pub.publisher.Trigger e

    /// All published telemetry events.
    let events (pub: TracePublisher) : IEvent<TelemetryEvent> = pub.publisher.Publish