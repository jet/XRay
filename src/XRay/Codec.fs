/// <summary>Xray codecs.</summary>
/// <remarks>
/// Xray uses codecs to propagate trace data across services.
/// Specifically, when sending a message, a codec is used to embed trace data into the message.
/// Likewise, when receiving a message, a codec is used to extract trace data.
/// </remarks>
[<RequireQualifiedAccess>]
module XRay.Codec

/// Creates a codec.
let inline create (d: Dec<'i, 'a>) (e: Enc<'o, 'a>) : Codec<'i, 'o, 'a> = d, e

let decoder (c: Codec<'i, 'o, 'a>) : Dec<'i, 'a> = let dec, _ = c in dec
let encoder (c: Codec<'i, 'o, 'a>) : Enc<'o, 'a> = let _, enc = c in enc


/// Additional operations on Choice
[<RequireQualifiedAccess>]
module Choice =
    let result x = Choice1Of2 x
    let throw  x = Choice2Of2 x
    let apply f x = match f, x with Choice1Of2 a, Choice1Of2 b              -> Choice1Of2 (a b) | Choice2Of2 e, _ | _, Choice2Of2 e        -> Choice2Of2 e: Choice<'b,'e>
    let map   f                        = function Choice1Of2 v              -> Choice1Of2 (f v) | Choice2Of2 e                             -> Choice2Of2 e
    let flatten                        = function Choice1Of2 (Choice1Of2 v) -> Choice1Of2 v     | Choice1Of2 (Choice2Of2 e) | Choice2Of2 e -> Choice2Of2 e
    let bind (f:'t -> _)               = function Choice1Of2 v              -> f v              | Choice2Of2 e                             -> Choice2Of2 e: Choice<'v,'e>
    let inline catch (f:'t -> _)       = function Choice1Of2 v              -> Choice1Of2 v     | Choice2Of2 e                             -> f e         : Choice<'v,'e>
    let inline either f g              = function Choice1Of2 v              -> f v              | Choice2Of2 e                             -> g e
    let inline orElse y x = match x, y with Choice1Of2 _, _ -> x | Choice2Of2 x, Choice2Of2 y -> Choice2Of2 ((+) x y) | _, _ -> y

/// Additional operations on Result
[<RequireQualifiedAccess>]
module Result =
    let result x = Ok x
    let throw  x = Error x
    let apply f x = match f, x with Ok a, Ok b              -> Ok (a b) | Error e, _ | _, Error e        -> Error e: Result<'b,'e>
    let map   f                        = function Ok v              -> Ok (f v) | Error e                             -> Error e
    let flatten                        = function Ok (Ok v) -> Ok v     | Ok (Error e) | Error e -> Error e
    let bind (f:'t -> _)               = function Ok v              -> f v              | Error e                             -> Error e: Result<'v,'e>
    let inline catch (f:'t -> _)       = function Ok v              -> Ok v     | Error e                             -> f e         : Result<'v,'e>
    let inline either f g              = function Ok v              -> f v              | Error e                             -> g e
    let inline orElse y x = match x, y with Ok _, _ -> x | Error x, Error y -> Error ((+) x y) | _, _ -> y

/// Xray Json codecs.
type Json<'JsonValue> (tryGetProperty, setField, jnull) =
   
    member __.strongEmbeddedFieldDecode
        (embeddedFieldName: string)
        (ctxDec: Dec<'JsonValue, 'c>)
        (dec: Dec<'JsonValue, 'a>) : Dec<'JsonValue, 'a * 'c> =
        let dec json =
            json
            |> Dec.decode dec
            |> Result.bind (fun a ->
                match tryGetProperty embeddedFieldName json with
                | Some json -> json |> Dec.decode ctxDec |> Result.map (fun c -> a, c)
                | None -> Error (sprintf "property '%s' not found" embeddedFieldName))
        Dec.create dec

    /// Decoder that embeds lenient context trace decoder,
    /// i.e., the one which may generate new traces in case of absence in the original message
    member __.embeddedFieldDecodeWithFallback
        (embeddedFieldName: string)
        (ctxDec: Dec<'JsonValue,'c>)
        (dec: Dec<'JsonValue,'a>) : Dec<'JsonValue,'a*'c> =
        let dec json =
            json
            |> Dec.decode dec
            |> Result.bind (fun a ->
                match tryGetProperty embeddedFieldName json with
                    | Some json -> json
                    | None -> jnull
                |> Dec.decode ctxDec |> Result.map (fun c -> a,c))
        Dec.create dec
    
    member __.strongEmbeddedFieldEncode
        (embeddedFieldName: string)
        (ctxEnc: Enc<'JsonValue, 'c>)
        (enc: Enc<'JsonValue, 'a>) : Enc<'JsonValue, 'a * 'c> =
        let enc (a, c) =
            let json = Enc.encode enc a in
            let ctxJson = Enc.encode ctxEnc c in      
            json |> setField embeddedFieldName ctxJson
        Enc.create enc
    
    /// Creates a strong JsonValue codec by embedding data into a field of the JsonValue record.
    member this.strongEmbeddedField
        (embeddedFieldName: string)
        (ctxCodec: Codec<'JsonValue, 'c>)
        (codec: Codec<'JsonValue, 'a>) : Codec<'JsonValue, 'a * 'c> =
            let dec = this.strongEmbeddedFieldDecode embeddedFieldName (ctxCodec |> decoder) (codec |> decoder)
            let enc = this.strongEmbeddedFieldEncode embeddedFieldName (ctxCodec |> encoder) (codec |> encoder)
            create dec enc
    
    /// Creates a lenient JsonValue codec by embedding data into a field of the JsonValue record.
    /// Context codec must be able to parse nulls, empty or invalid JSON objects, returning some valid TraceContext
    member this.embeddedFieldWithFallback
        (embeddedFieldName: string)
        (ctxCodec: Codec<'JsonValue, 'c>)
        (codec: Codec<'JsonValue,'a>) : Codec<'JsonValue, 'a * 'c>=
            let dec = this.embeddedFieldDecodeWithFallback embeddedFieldName (ctxCodec |> decoder) (codec |> decoder)
            let enc = this.strongEmbeddedFieldEncode embeddedFieldName (ctxCodec |> encoder) (codec |> encoder)
            create dec enc
    
    /// Creates a strong JsonValue codec by embedding data into the JsonValue record.
    /// This can be used to carry a TraceContext.
    member this.strongEmbeddedFieldDefault ctxCodec codec = this.strongEmbeddedField "~trace" ctxCodec codec
    
    member this.embeddedFieldDefaultWithFallback ctxCodec codec = this.embeddedFieldWithFallback "~trace" ctxCodec codec


type JsonCtx<'JsonValue> (tryGetProperty, setField, jnull, ctxCodec) =
    inherit Json<'JsonValue> (tryGetProperty, setField, jnull)

    /// A JSON codec for the TraceContext.
    member __.TraceContext : Codec<'JsonValue, TraceContext> = ctxCodec
    
    /// JSON Codec that calls a provider for a new value when decoder fails
    member this.TraceContextWithFallback (provider: unit -> TraceContext) : Codec<'JsonValue, TraceContext> =
        let decoder = this.TraceContext |> decoder
        let dec json = json |> Dec.decode decoder |> Result.orElse (provider() |> Ok)
        let enc = this.TraceContext |> encoder
        create dec enc
    
    /// Creates a codec for a pair of a type and TraceContext using a type's existing Json codec.
    member this.embeddedFieldDefault codec : Codec<'JsonValue, _ * TraceContext> =
        this.embeddedFieldDefaultWithFallback
            (this.TraceContextWithFallback XRay.TraceContext.originateNew)
            (codec)


open System.Net.Http
open System.Net.Http.Headers

/// Xray HTTP codecs.
type Http (ctxCodec) =

    /// "Jet-Dr-Orpheus"    
    let [<Literal>] HTTP_HEADER_NAME = "Jet-Dr-Orpheus"
    
    member internal __.traceContextToJsonString (tc:TraceContext) : string =
      tc |> (ctxCodec |> encoder)
    
    member internal __.tryParseTraceContxtFromJsonString (str:string) : Result<TraceContext, string> =
      str |> (ctxCodec |> decoder)
    
    member private this.extract (fieldName:string) (headers:HttpHeaders) : Result<TraceContext, string> =
      let mutable result = Unchecked.defaultof<_>
      if headers.TryGetValues(fieldName, &result) then
          match result |> Seq.tryPick Some with
          | Some header -> header |> this.tryParseTraceContxtFromJsonString
          | None -> Error "Header not found"
      else
          Error "Header not found"
    
    member private this.inject (fieldName:string) (t:TraceContext) (headers:HttpHeaders) =
        headers.Add(fieldName, (this.traceContextToJsonString t : string))
    
    member this.extractReq (fieldName:string) (req:HttpRequestMessage) : Result<TraceContext, string> =
        this.extract fieldName req.Headers
    
    member this.extractRes (fieldName:string) (res:HttpResponseMessage) : Result<TraceContext, string> =
        this.extract fieldName res.Headers
    
    member this.injectReq (fieldName:string) (t:TraceContext) (req:HttpRequestMessage) : HttpRequestMessage =
        this.inject fieldName t req.Headers
        req
    
    member this.injectRes (fieldName:string) (t:TraceContext) (res:HttpResponseMessage) : HttpResponseMessage =
        this.inject fieldName t res.Headers
        res
    
    /// HTTP request as a carrier.
    member this.reqHeader (headerName:string) : Codec<HttpRequestMessage, HttpRequestMessage * TraceContext> =
        create
            (fun req -> this.extractReq headerName req |> Result.map (fun t -> req,t))
            (fun (req,trace) -> this.injectReq headerName trace req) 
    
    /// HTTP request as a carrier.
    member this.resHeader (headerName:string) : Codec<HttpResponseMessage, HttpResponseMessage * TraceContext> =
        create
            (fun req -> this.extractRes headerName req |> Result.map (fun t -> req,t))
            (fun (req,trace) -> this.injectRes headerName trace req)    
    
    /// HTTP request as a carrier using HTTP_HEADER_NAME.
    member this.Req : Codec<HttpRequestMessage, HttpRequestMessage * TraceContext> = this.reqHeader HTTP_HEADER_NAME
    
    /// HTTP response as a carrier using HTTP_HEADER_NAME.
    member this.Res : Codec<HttpResponseMessage, HttpResponseMessage * TraceContext> = this.resHeader HTTP_HEADER_NAME