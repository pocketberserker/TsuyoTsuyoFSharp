module TsuyoGame

open System
open System.Collections.Generic
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.Windows.Forms
open System.Windows.Forms.Integration
open System.Windows
open System.Windows.Controls
open System.Windows.Media.Imaging
open Twitterizer
open TsuyoTsuyo

type TsuyoGame() as this =
  inherit Game()

  let gameTitle = "つよつよえふしゃ～ぷ！"
  let frameWidth,frameHeight= 800,700
  let tsuyoWidth,tsuyoHeight = 48,48
  let initPointX,initPointY = frameWidth/4,frameHeight/7
  let graphicsDeviceManager = new GraphicsDeviceManager(this)
  let sprite = lazy new SpriteBatch(this.GraphicsDevice)
  let font = lazy this.Content.Load<SpriteFont>("Font")

  let fps = 60.

  let mutable ps = createTsuyoObj twitStatusList

  let mutable textures:(string * Lazy<Texture2D>) list = ["##dummy##",lazy this.Content.Load<Texture2D>("480_16colors_normal")]
  let mutable bitmaps = ["##dummy##",BitmapFrame.Create(new System.Uri("480_16colors_normal.png", System.UriKind.Relative))]

  let viewmodel = new TweetViewModel.ViewModel()

  let created x =

    let asyncLoadBitmapFrame (status:TwitterStatus) =
      async {
        status
        |> (fun x ->
          if bitmaps |> List.exists (fun (k,v) -> k = x.User.ScreenName) |> not then
            use stream = x |> TsuyoType.Real |> tryGetStream |> Option.get
            bitmaps <- (x.User.ScreenName, BitmapFrame.Create(stream))::bitmaps)
      } |> Async.Start

    twitStatusList <- Some x :: (twitStatusList |> List.rev) |> List.rev
    x |> asyncLoadBitmapFrame

  let start () = TwitStream.start "" created

  let drawTsuyo (tsuyo:Tsuyo) =
    let fx,fy = float32 (initPointX+tsuyo.Pos%RowNum*tsuyoWidth), float32 (initPointY+(tsuyo.Pos/RowNum-1)*tsuyoHeight)
    textures
    |> List.tryPick (fun (k,v) -> if k = tsuyo.ScreenName then Some (k,v) else None)
    |> Option.iter (fun (_,v) -> sprite.Force().Draw(v.Force(), Vector2(fx,fy), Color.White))
  
  let updateNextTsuyo (statusList:TwitterStatus option list) =
    
    let items = new List<TweetModel.Model>()

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
      | Microsoft.Xna.Framework.Input.Keys.Z -> rotateL ps
      | Microsoft.Xna.Framework.Input.Keys.X -> rotateR ps
      | Microsoft.Xna.Framework.Input.Keys.Left ->
        ps |> function
          | CollideLeftWall -> ps
          | _ -> if detectCollision (ps.Tsuyo1.Pos-1) || detectCollision (ps.Tsuyo2.Pos - 1) then ps else move ps Direction.Left
      | Microsoft.Xna.Framework.Input.Keys.Right ->
        ps |> function
          | CollideRightWall -> ps
          | _ -> if detectCollision (ps.Tsuyo1.Pos+1) || detectCollision (ps.Tsuyo2.Pos + 1) then ps else move ps Direction.Right
      | Microsoft.Xna.Framework.Input.Keys.Down ->
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
              let texture = lazy Texture2D.FromStream(this.GraphicsDevice, x |> TsuyoType.Real |> tryGetStream |> Option.get)
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

  do
    this.Content.RootDirectory <- "TsuyoTsuyoContent"
    this.Window.Title <- gameTitle
    (frameWidth,frameHeight) ||>
      fun x y ->
        graphicsDeviceManager.PreferredBackBufferWidth <- x
        graphicsDeviceManager.PreferredBackBufferHeight <- y
    1. / fps |> fun sec -> this.TargetElapsedTime <- TimeSpan.FromSeconds sec

  override game.Initialize () =
    graphicsDeviceManager.GraphicsProfile <- GraphicsProfile.HiDef
    graphicsDeviceManager.ApplyChanges()
    let elementHost = new ElementHost()
    elementHost.Location <- new System.Drawing.Point(0,0)
    elementHost.Size <- new System.Drawing.Size(800,96)
    let control = Application.LoadComponent(new System.Uri("/tsuyofs;component/TweetView.xaml", System.UriKind.Relative)) :?> UserControl
    control.DataContext <- viewmodel
    elementHost.Child <- control
    Control.FromHandle(game.Window.Handle).Controls.Add elementHost
    base.Initialize()

  override game.BeginRun () = async { start () } |> Async.Start

  override game.Update gameTime =
    operateKeys () ||> update
    base.Update gameTime

  override game.Draw gameTime =
    sprite.Force().Begin()
    ps.Tsuyo1 :: ps.Tsuyo2 :: fieldTsuyo |> List.filter (fun (x:Tsuyo) -> x.Hidden |> not) |> List.iter drawTsuyo
    updateNextTsuyo twitStatusList
    sprite.Force().End()
    base.Draw gameTime

  override game.EndRun () =
    textures |> List.iter (fun (_,v) -> v.Force().Dispose())
    base.EndRun()

module Program =
  [<STAThread>]
  [<EntryPoint>]
  let main args =
    use game = new TsuyoGame() in
      game.Run()
    0