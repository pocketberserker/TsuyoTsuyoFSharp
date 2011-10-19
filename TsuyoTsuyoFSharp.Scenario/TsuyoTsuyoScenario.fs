module TsuyoTsuyoScenario

open NUnit.Framework
open NaturalSpec
open TsuyoTsuyo

module 周りに障害物がない =

  module 第2つよを右回転 =

    [<Scenario>]
    let ``初期状態時の右回転はつよが右に移動する``() =
      let tobj = new TsuyoObj(None,None)

      Given tobj
      |> When rotateR
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Right
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum+RowNum/2)
      |> Verify

    [<Scenario>]
    let ``第2つよが右にある時の右回転はつよが下に移動する``() =
      let tobj = new TsuyoObj(None,None)
      (fst tobj.Tsuyo2).Pos <- RowNum+RowNum/2
      tobj.Tsuyo2 <- (fst tobj.Tsuyo2), SndTsuyoPos.Right

      Given tobj
      |> When rotateR
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Down
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2+2)
      |> Verify
      
    [<Scenario>]
    let ``第2つよが下にある時の右回転はつよが左に移動する``() =
      let tobj = new TsuyoObj(None,None)
      (fst tobj.Tsuyo2).Pos <- RowNum+RowNum/2-2
      tobj.Tsuyo2 <- (fst tobj.Tsuyo2), SndTsuyoPos.Down
    
      Given tobj
      |> When rotateR
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Left
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum+RowNum/2-2)
      |> Verify

    [<Scenario>]
    let ``第2つよが左にある時の右回転はつよが最初の位置に戻る``() =
      let tobj = new TsuyoObj(None,None)
      (fst tobj.Tsuyo2).Pos <- RowNum+RowNum/2-2
      tobj.Tsuyo2 <- (fst tobj.Tsuyo2), SndTsuyoPos.Left
    
      Given tobj
      |> When rotateR
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Up
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum/2-1)
      |> Verify
  