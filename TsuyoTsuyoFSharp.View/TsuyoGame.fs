module TsuyoGame

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
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

  let drawTsuyo (tsuyo:Tsuyo) =
    let fx,fy = float32 (initPointX+tsuyo.Pos%RowNum*tsuyoWidth), float32 (initPointY+(tsuyo.Pos/RowNum-1)*tsuyoHeight)
    textures
    |> List.tryPick (fun (k,v) -> if k = tsuyo.ScreenName then Some (k,v) else None)
    |> Option.iter (fun (_,v) -> sprite.Force().Draw(v.Force(), Vector2(fx,fy), Color.White))
      
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

    let asyncLoadContent (tsuyo1:Tsuyo) (tsuyo2:Tsuyo) =
      async {
        [tsuyo1;tsuyo2]
        |> List.iter (fun x ->
          if textures |> List.exists (fun (k,v) -> k = x.ScreenName) |> not then
            let texture = lazy Texture2D.FromStream(this.GraphicsDevice, x.ImageStream.Value) in textures <- (x.ScreenName ,texture)::textures)
      } |> Async.Start

    if detectCollision (fst.Pos+RowNum) || detectCollision (snd.Pos+RowNum) then
      ps.Tsuyo2Pos |> function
        | SndTsuyoPos.Right | SndTsuyoPos.Left -> (fst,snd) ||> fun x y -> (fall x), (fall y)
        | _ -> fst,snd
      |> fun (x,y) -> ps <- createTsuyoObj twitStatusList; asyncLoadContent ps.Tsuyo1 ps.Tsuyo2; [x;y] |> List.iter erase
    else
      ps |> function
        | CollideBottom -> ps <- createTsuyoObj twitStatusList; asyncLoadContent ps.Tsuyo1 ps.Tsuyo2; [fst;snd] |> List.iter erase
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
    base.Initialize()

  override game.BeginRun () = async { start () } |> Async.Start

  override game.Update gameTime =
    operateKeys () ||> update
    base.Update gameTime

  override game.Draw gameTime =
    sprite.Force().Begin()
    ps.Tsuyo1 :: ps.Tsuyo2 :: fieldTsuyo |> List.filter (fun (x:Tsuyo) -> x.Hidden |> not) |> List.map drawTsuyo |> ignore
    twitStatusList |> List.filter (fun x -> x.IsSome)
    |> List.map (fun x -> sprite.Force().DrawString(font.Force(),x.Value.User.ScreenName,Vector2(300.f,300.f),Color.White)) |> ignore
    sprite.Force().End()
    base.Draw gameTime

  override game.EndRun () =
    textures |> List.iter (fun (_,v) -> v.Force().Dispose())
    base.EndRun()

module Program =
  [<EntryPoint>]
  let main args =
    use game = new TsuyoGame() in
      game.Run()
    0