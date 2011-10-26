module TwitStream

open System
open System.IO
open System.Net
open System.Reflection
open Twitterizer
open Twitterizer.Streaming

let readTokenData path =
  use reader = File.OpenText path
  reader.ReadToEnd().Split([|','|])

let tokenData = readTokenData "token.txt"

let userAgent = tokenData.[0]
let token = new OAuthTokens()

let settingToken (token:OAuthTokens)  =
  token.set_ConsumerKey(tokenData.[1])
  token.set_ConsumerSecret(tokenData.[2])
  token.set_AccessToken(tokenData.[3])
  token.set_AccessTokenSecret(tokenData.[4])

// twitterizer内のURLが最新のStreaming APIに対応していないためstartPublicStreamを自作
let startMyPublicStream (stream:TwitterStream) stopped created =
  
  let setFieldData name data =
    let field = stream.GetType().GetField(name, BindingFlags.Instance ||| BindingFlags.NonPublic)
    field.SetValue(stream, data)

  let builder = new WebRequestBuilder(new Uri("https://stream.twitter.com/1/statuses/filter.json"), HTTPVerb.POST, stream.Tokens, true, userAgent)
  
  let prepareStreamOptions = stream.GetType().GetMethod("PrepareStreamOptions", BindingFlags.Instance ||| BindingFlags.NonPublic)
  prepareStreamOptions.Invoke(stream, [|builder|]) |> ignore
  
  let request = builder.PrepareRequest()

  setFieldData "streamStoppedCallback" stopped
  setFieldData "statusCreatedCallback" created
  setFieldData "stopReceived" false

  let StreamCallback = stream.GetType().GetMethod("StreamCallback", BindingFlags.Instance ||| BindingFlags.NonPublic)  
  let callback = new AsyncCallback (fun result -> StreamCallback.Invoke(stream, [|result|]) |> ignore)
  
  request.BeginGetResponse(callback, request) 

let startStream token streamMode track created =

  settingToken token

  let opts = new StreamOptions()
  if track |> String.IsNullOrEmpty |> not then opts.Track.Add track

  use stream = new TwitterStream(token, userAgent, opts)
  let stCreatedCallback = new StatusCreatedCallback(created)
  let stStreamStoppedCallback = new StreamStoppedCallback (fun _ -> ())

  streamMode stream stStreamStoppedCallback stCreatedCallback |> ignore

let publicStream stream stopped created = startMyPublicStream stream stopped created

let userStream (stream:TwitterStream) stopped created = stream.StartUserStream(null, stopped, created, null, null, null, null)

let start track created = startStream token userStream track created