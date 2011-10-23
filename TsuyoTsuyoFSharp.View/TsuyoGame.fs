module TsuyoGame

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open TsuyoTsuyo

type TsuyoGame() as this =
  inherit Game()

  let gameTitle = "つよつよえふしゃ～ぷ！"
  let graphicsDeviceManager = new GraphicsDeviceManager(this)
  let sprite = lazy new SpriteBatch(this.GraphicsDevice)

  let fps = 60.

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
    base.Update gameTime

  override game.Draw gameTime =
    sprite.Force().Begin()
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