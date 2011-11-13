module TweetModel

open System
open System.Diagnostics
open System.ComponentModel
open System.Windows.Media.Imaging

/// Model
[<Sealed>]
[<DebuggerStepThrough>]
type Model () = 
  let mutable _image:BitmapFrame = null
  let mutable _text = ""
  member this.Image
    with get() = _image
     and set(value) = _image <- value
  member this.Text 
    with get() = _text 
     and set(value) = _text <- value

