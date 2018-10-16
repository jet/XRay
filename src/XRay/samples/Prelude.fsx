#r "System.Net.Http"
#r @"..\..\..\tests\XRay.Tests\bin\Debug\net461\FSharpPlus.dll"
#r @"..\..\..\tests\XRay.Tests\bin\Debug\net461\Newtonsoft.Json.dll"
#r @"..\..\..\tests\XRay.Tests\bin\Debug\net461\Fleece.NewtonsoftJson.dll"

#r "../../../bin/XRay/net451/XRay.dll"


open System
open System.Net
open System.Net.Http

let logInfo  x = printfn x
let logError x = printfn x

open System.Threading.Tasks
type HttpReq = HttpRequestMessage
type HttpRes = HttpResponseMessage

module AsyncOps =
      let awaitTaskCorrect (t:Task<'a>) : Async<'a> =
        Async.FromContinuations <| fun (ok,err,cnc) ->
          t.ContinueWith (fun (t:Task<'a>) ->
            if t.IsFaulted then
                let e = t.Exception
                if e.InnerExceptions.Count = 1 then err e.InnerExceptions.[0]
                else err e
            elif t.IsCanceled then err (TaskCanceledException("Task wrapped with Async has been cancelled."))
            elif t.IsCompleted then ok t.Result
            else err(Exception "invalid Task state!")
          )
          |> ignore


type Async with
        /// Asynchronously await supplied task with the following variations:
        ///     *) Task cancellations are propagated as exceptions
        ///     *) Singleton AggregateExceptions are unwrapped and the offending exception passed to cancellation continuation
        static member inline AwaitTaskCorrect (task:Task<'T>) : Async<'T> = AsyncOps.awaitTaskCorrect task

        /// Raises supplied exception using Async's exception continuation directly.
        static member Raise<'T> (e : exn) : Async<'T> = Async.FromContinuations(fun (_,ec,_) -> ec e)


module HttpReq =
    let inline create () = new HttpReq()

    /// Assigns a method to an HTTP request.
    let inline withMethod (m:HttpMethod) (req:HttpReq) =
      req.Method <- m
      req

    /// Assigns a string body to an HTTP request.
    let inline withBodyString (s:string) (req:HttpReq) =
      req.Content <- new StringContent(s)
      req

    /// Creates an HTTP POST request.
    let inline post () = create () |> withMethod HttpMethod.Post

module HttpRes =
    open System.Text

    let inline ofStatusCode (s:HttpStatusCode) = let res = new HttpRes(s) in res
    let inline withContent (s:#HttpContent) (res:HttpRes) = res.Content <- s; res
    let inline withBodyString (s:string) (e:Encoding) (mediaType:string) = withContent (new StringContent(s, e, mediaType))
    let inline withBodyStringUTF8 (s:string) (mediaType:string) = withBodyString s Encoding.UTF8 mediaType

module HttpClient =

    /// Creates an HTTP client arrow given System.Net.Http.HttpClient.
    /// Same as toArrow.
    let ofClient (c:HttpClient) (req:HttpReq) : Async<HttpRes> = async {
        let! ct = Async.CancellationToken
        try return! c.SendAsync(req, ct) |> Async.AwaitTaskCorrect
        with
        | :? System.Threading.Tasks.TaskCanceledException ->
            // Addresses a known issue with System.Net.Http.HttpClient in which timeout errors
            // for async requests are surfaced as cancelled tasks. Rephrase as a proper error message.
            let uri = req.RequestUri.GetLeftPart UriPartial.Path
            let timeoutExn = TimeoutException(sprintf "An HTTP request to RequestUri=%O has timed out." uri)
            return! Async.Raise timeoutExn
      }
    
    
    /// Creates an HTTP client for the specified host url.
    let baseAddressUri (host:Uri) =
        let client = new HttpClient(BaseAddress = host)
        ofClient client
    
    /// Creates an HTTP client for the specified host url.
    let inline baseAddress (host:string) =
        baseAddressUri (Uri(host))
        

module HttpHost =

    // let private Log = Marvel.Log.create "Marvel.Http"

    module Config =
        open System.Threading

        let set max =
            ServicePointManager.DefaultConnectionLimit <- max
            let minWorkerThreads,minIoThreads = ref 0, ref 0
            ThreadPool.GetMinThreads(minWorkerThreads, minIoThreads)
            ThreadPool.SetMinThreads(!minWorkerThreads, max)

    type HttpListener with member this.AsyncGetContext() = Async.FromBeginEnd(this.BeginGetContext, this.EndGetContext)

    let hostAsync uriPrefix (service:HttpReq -> Async<HttpRes>) =

        let listener = new HttpListener()
        listener.Prefixes.Add(uriPrefix)
        listener.AuthenticationSchemes <- AuthenticationSchemes.Anonymous
        listener.Start()

        let asHttpRequest (ctx:HttpListenerContext) =
            let request = new HttpRequestMessage(new HttpMethod(ctx.Request.HttpMethod), ctx.Request.Url)
            request.Version <- ctx.Request.ProtocolVersion
            request.Content <- new StreamContent(ctx.Request.InputStream)
            for i = 0 to (ctx.Request.Headers.Count - 1) do
                let name = ctx.Request.Headers.GetKey(i)
                let values = ctx.Request.Headers.GetValues(i)
                if not (isNull name || name.StartsWith("Content")) then request.Headers.Add(name, values)
                else request.Content.Headers.Add(name, values)
            request

        let fill (response:HttpRes) (ctx:HttpListenerContext) = async {
            ctx.Response.StatusCode <- (int response.StatusCode)
            for pair in response.Headers do
                if not (isNull pair.Value) then
                    for value in pair.Value do
                        ctx.Response.Headers.Add(pair.Key, value)
            if not (isNull response.Content) then
                if not (isNull response.Content.Headers) then
                    for pair in response.Content.Headers do
                        for value in pair.Value do
                            if not (isNull value || value.StartsWith "Content") then
                                ctx.Response.Headers.Add(pair.Key, value)
                    do! response.Content.CopyToAsync(ctx.Response.OutputStream) |> Async.AwaitTask }

        let handle (ctx:HttpListenerContext) = async {
            try
                try
                    use request = asHttpRequest ctx
                    use! response = service request
                    do! fill response ctx
                finally
                    ctx.Response.Close()
            with ex ->
                logError "ERROR={%A}" ex
        }

        let ctx = listener.AsyncGetContext()

        let worker = async {
            while true do
                try
                    let! ctx = ctx
                    Async.Start (handle ctx)
                with ex ->
                    logError "ERROR={%A}" ex
        }

        logInfo "Listening to={%s}" uriPrefix

        let close () =
          logInfo "Closing HttpListener=%s" uriPrefix
          (listener :> IDisposable).Dispose()

        async.TryFinally(worker, close)



module CtxCodecNewtonsoft =
    open FSharpPlus
    open Fleece.Newtonsoft
    open Fleece.Newtonsoft.Operators
    open XRay
    

    let decodeTraceCtx (js: Newtonsoft.Json.Linq.JToken) : ParseResult<TraceContext> =
        match js with
        | JObject o -> monad {
            let! trace_id   =  o .@ "tid"
            let! trace_tags = (o .@ "ts") |> Result.map TraceTags.ofMap
            return {
                TraceId = trace_id
                Tags = trace_tags }}
        | x -> Helpers.failparse "TraceContext" x
    
    let encodeTraceCtx (t: TraceContext) : Newtonsoft.Json.Linq.JToken =
        jobj [|
            jpair "tid" t.TraceId
            jpair "ts" (TraceTags.toMap t.Tags)
        |]
        
    let jsonValueCodecT = decodeTraceCtx, encodeTraceCtx