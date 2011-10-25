module TsuyoGame

open System
open System.Collections.Generic
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open TsuyoTsuyo

type TsuyoGame() as this =
  inherit Game()

  let gameTitle = "つよつよえふしゃ～ぷ！"
  let graphicsDeviceManager = new GraphicsDeviceManager(this)
  let sprite = lazy new SpriteBatch(this.GraphicsDevice)

  let fps = 60.

  let mutable ps = new TsuyoObj(None,None)

  let textureSet = new Dictionary<string,Lazy<Texture2D>>()

  let drawTsuyo (tsuyo:Tsuyo) =
    let fx,fy = tsuyo.Pos%RowNum, tsuyo.Pos/RowNum
    let lx,ly = float32 (fx*48),float32 (fy*48-48)
    if textureSet.ContainsKey tsuyo.ScreenName then sprite.Force().Draw(textureSet.Item(tsuyo.ScreenName).Force(), Vector2(lx,ly), Color.White)
    else
      let texture = lazy Texture2D.FromStream(this.GraphicsDevice, tsuyo.ImageStream)
      sprite.Force().Draw(texture.Force(), Vector2(lx,ly), Color.White)
      textureSet.Add(tsuyo.ScreenName ,texture)
      
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
    |> List.map operateKey
    |> ignore

  do
    this.Window.Title <- gameTitle
    (800,600) ||>
      fun x y ->
        graphicsDeviceManager.PreferredBackBufferWidth <- x
        graphicsDeviceManager.PreferredBackBufferHeight <- y
    1. / fps |> fun sec -> this.TargetElapsedTime <- TimeSpan.FromSeconds sec

  override game.Initialize () =
    base.Initialize()

  override game.Update gameTime =
    operateKeys ()
    if detectCollision (ps.Tsuyo1.Pos+RowNum) || detectCollision (ps.Tsuyo2.Pos+RowNum) then
      fieldTsuyo <- ps.Tsuyo1::ps.Tsuyo2::fieldTsuyo; ps <- new TsuyoObj(None,None)
    else
      ps |> function
        | CollideBottom -> fieldTsuyo <- ps.Tsuyo1::ps.Tsuyo2::fieldTsuyo; ps <- new TsuyoObj(None,None)
        | _ -> ()
    base.Update gameTime

  override game.Draw gameTime =
    sprite.Force().Begin()
    ps.Tsuyo1 :: ps.Tsuyo2 :: fieldTsuyo |> List.map drawTsuyo |> ignore
    sprite.Force().End()
    base.Draw gameTime

  override game.EndRun () =
    base.EndRun()

module Program =
  [<EntryPoint>]
  let main args =
    use game = new TsuyoGame() in
      game.Run()
    0