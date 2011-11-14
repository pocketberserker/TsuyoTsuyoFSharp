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

let startStream token streamMode track created =

  settingToken token

  let opts = new StreamOptions()
  if track |> String.IsNullOrEmpty |> not then opts.Track.Add track

  use stream = new TwitterStream(token, userAgent, opts)
  let stCreatedCallback = new StatusCreatedCallback(created)
  let stStreamStoppedCallback = new StreamStoppedCallback (fun _ -> ())

  streamMode stream stStreamStoppedCallback stCreatedCallback |> ignore

  while true do ()

let publicStream (stream:TwitterStream) stopped created = stream.StartPublicStream(stopped, created, null, null, null)

let userStream (stream:TwitterStream) stopped created = stream.StartUserStream(null, stopped, created, null, null, null, null)

let start track created = startStream token userStream track created