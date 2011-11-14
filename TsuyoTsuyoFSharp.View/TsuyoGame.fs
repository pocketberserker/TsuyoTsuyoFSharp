module TsuyoGame

open System
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Threading
open System.Windows.Interop
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.Windows.Media.Imaging
open Twitterizer
open TsuyoTsuyo

type TsuyoGame(viewmodel:TweetViewModel.ViewModel) as this =
  inherit UserControl()

  let gameTitle = "つよつよえふしゃ～ぷ！"
  let frameWidth,frameHeight= 800,600
  let tsuyoWidth,tsuyoHeight = 48,48
  let initPointX = frameWidth/4
  let fps = 60.

  let handle = lazy (HwndSource.FromVisual(this) :?> HwndSource).Handle
  let mutable graphicsDevice =
    lazy begin
      let pp = new PresentationParameters()
      (frameWidth,frameHeight) ||> fun x y -> pp.BackBufferWidth <- x; pp.BackBufferHeight <- y
      pp.BackBufferFormat <- SurfaceFormat.Color
      pp.DeviceWindowHandle <- handle.Force()
      pp.DepthStencilFormat <- DepthFormat.Depth24Stencil8
      pp.IsFullScreen <- false
      new GraphicsDevice(GraphicsAdapter.DefaultAdapter,GraphicsProfile.HiDef, pp)
    end

  let mutable sprite = lazy new SpriteBatch(graphicsDevice.Force())

  let mutable ps = createTsuyoObj twitStatusList

  let mutable textures:(string * Lazy<Texture2D>) list = ["##dummy##",lazy Texture2D.FromStream(graphicsDevice.Force(),"480_16colors_normal.png" |> File.OpenRead)]

  let mutable bitmaps = ["##dummy##",BitmapFrame.Create(new System.Uri("480_16colors_normal.png", System.UriKind.Relative))]
  let mutable streams = []

  let created x =

    let asyncLoadBitmapFrame (status:TwitterStatus) =
      async {
        status
        |> (fun x ->
          if bitmaps |> List.exists (fun (k,v) -> k = x.User.ScreenName) |> not then
            let stream = x |> TsuyoType.Real |> tryGetStream |> Option.get
            streams <- stream::streams
            bitmaps <- (x.User.ScreenName, BitmapFrame.Create(stream))::bitmaps)
      } |> Async.Start

    twitStatusList <- Some x :: (twitStatusList |> List.rev) |> List.rev
    x |> asyncLoadBitmapFrame

  let start () = TwitStream.start "" created

  let drawTsuyo (tsuyo:Tsuyo) =
    let fx,fy = float32 (initPointX+tsuyo.Pos%RowNum*tsuyoWidth), float32 ((tsuyo.Pos/RowNum-1)*tsuyoHeight)
    if tsuyo.Hidden then ()
    textures
    |> List.tryPick (fun (k,v) -> if k = tsuyo.ScreenName then Some (k,v) else None)
    |> Option.iter begin 
        fun (_,v) ->
          let scale = new Vector2(float32 tsuyoWidth / float32 (v.Force().Width), float32 tsuyoHeight / float32 (v.Force().Height))
          sprite.Force().Draw(v.Force(), Vector2(fx,fy), Nullable(), Color.White, 0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)
      end
  
  let updateNextTsuyo (statusList:TwitterStatus option list) =
    
    let items = new System.Collections.Generic.List<TweetModel.Model>()

    let draw' = function
      | TsuyoType.Real status ->
        status |> (fun s ->
          bitmaps |> List.tryPick (fun (k,v) -> if k = s.User.ScreenName then Some (k,v) else None)
          |> Option.iter (fun (_,v) -> let model = new TweetModel.Model() in model.Image <- v; model.Text <- s.Text; items.Add model))
      | TsuyoType.Dummy ->
        bitmaps |> List.tryPick (fun (k,v) -> if k = "##dummy##" then Some (k,v) else None)
        |> Option.iter (fun (_,v) -> let model = new TweetModel.Model() in model.Image <- v; model.Text <- ""; items.Add model)

    match statusList with
    | [] -> [TsuyoType.Dummy; TsuyoType.Dummy] |> List.iter (fun x -> draw' x)
    | [x] -> x |> Option.iter (fun x -> x |> TsuyoType.Real |> draw'); draw' TsuyoType.Dummy
    | x1::x2::xs -> [x1;x2] |> List.iter (fun x -> x |> Option.iter (fun x -> x |> TsuyoType.Real |> draw'))
    items.Reverse()
    viewmodel.Items <- items
      
  let operateKeys () =
    let operateKey =
      function
      | Keys.Z -> rotateL ps
      | Keys.X -> rotateR ps
      | Keys.Left ->
        ps |> function
          | CollideLeftWall -> ps
          | _ -> if detectCollision (ps.Tsuyo1.Pos-1) || detectCollision (ps.Tsuyo2.Pos - 1) then ps else move ps Direction.Left
      | Keys.Right ->
        ps |> function
          | CollideRightWall -> ps
          | _ -> if detectCollision (ps.Tsuyo1.Pos+1) || detectCollision (ps.Tsuyo2.Pos + 1) then ps else move ps Direction.Right
      | Keys.Down ->
        move ps Direction.Down
      | _ -> ps
      
    Keyboard.GetState().GetPressedKeys()
    |> Array.toList
    |> List.map operateKey |> function | [] -> ps.Tsuyo1,ps.Tsuyo2 | list -> list |> List.head |> (fun (x:TsuyoObj) -> x.Tsuyo1,x.Tsuyo2)

  let update (fst:Tsuyo) (snd:Tsuyo) =

    let asyncLoadContents (statuses:TwitterStatus option list) =
      async {
        statuses
        |> List.iter (fun x ->
          x |> Option.iter (fun x ->
            if textures |> List.exists (fun (k,v) -> k = x.User.ScreenName) |> not then
              let texture = lazy Texture2D.FromStream(graphicsDevice.Force(), x |> TsuyoType.Real |> tryGetStream |> Option.get)
              textures <- (x.User.ScreenName ,texture)::textures))
      } |> Async.Start

    match twitStatusList with
    | [] -> ()
    | [x] -> [x] |> asyncLoadContents
    | x1::x2::xs -> [x1;x2] |> asyncLoadContents

    if detectCollision (fst.Pos+RowNum) || detectCollision (snd.Pos+RowNum) then
      ps.Tsuyo2Pos |> function
        | SndTsuyoPos.Right | SndTsuyoPos.Left -> (fst,snd) ||> fun x y -> (fall x), (fall y)
        | _ -> fst,snd
      |> fun (x,y) -> ps <- createTsuyoObj twitStatusList; [x;y] |> erase
    else
      ps |> function
        | CollideBottom -> ps <- createTsuyoObj twitStatusList; [fst;snd] |> erase
        | _ -> ()
    updateNextTsuyo twitStatusList

  let draw () =
    sprite.Force().Begin()
    ps.Tsuyo1 :: ps.Tsuyo2 :: fieldTsuyo |> List.filter (fun (x:Tsuyo) -> x.Hidden |> not) |> List.iter drawTsuyo
    sprite.Force().End()
    let rect = Nullable(new Rectangle(0, 125,frameWidth+183, frameHeight+105));
    graphicsDevice.Force().Present(Nullable(),rect,handle.Force())

  let beginRun () = async { start () } |> Async.Start

  let timer = new DispatcherTimer()
  do
    1. / fps |> TimeSpan.FromSeconds |> fun sec -> timer.Interval <- sec

  do timer.Tick
     |> Observable.subscribe (fun _ -> operateKeys() ||> update; draw())
     |> ignore

  do this.Dispatcher.ShutdownStarted
     |> Observable.subscribe (fun _ -> 
       timer.Stop()
       textures |> List.iter (fun (_,v) -> v.Force().Dispose())
       streams |> List.iter (fun s -> s.Close())
       graphicsDevice.Force().Dispose() )
     |> ignore

  do this.Loaded
     |> Observable.subscribe (fun _ -> beginRun (); timer.Start())
     |> ignore

module Program =
  [<STAThread>]
  [<EntryPoint>]
  let main args =
    let w = Application.LoadComponent(new System.Uri("/tsuyofs;component/Window.xaml", System.UriKind.Relative)) :?> Window
    let viewmodel = new TweetViewModel.ViewModel()
    w.DataContext <- viewmodel
    let content = w.FindName "content" :?> Grid
    new TsuyoGame(viewmodel) |> content.Children.Add |> ignore
    (new System.Windows.Application()).Run(w)