module TsuyoTsuyoScenario

open NUnit.Framework
open NaturalSpec
open TsuyoTsuyo

let changePos num dir =
  let tobj = new TsuyoObj(None,None)
  (fst tobj.Tsuyo2).Pos <- num
  tobj.Tsuyo2 <- (fst tobj.Tsuyo2), dir
  tobj

[<SetUpFixture>]
type SetUp() =
  [<SetUp>]
  member x.setup() = fieldTsuyo <- []

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
      let tobj = changePos (RowNum+RowNum/2) SndTsuyoPos.Right

      Given tobj
      |> When rotateR
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Down
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2+2)
      |> Verify
      
    [<Scenario>]
    let ``第2つよが下にある時の右回転はつよが左に移動する``() =
      let tobj = changePos (RowNum*2+2) SndTsuyoPos.Down
    
      Given tobj
      |> When rotateR
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Left
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum+RowNum/2-2)
      |> Verify

    [<Scenario>]
    let ``第2つよが左にある時の右回転はつよが最初の位置に戻る``() =
      let tobj = changePos (RowNum+RowNum/2-2) SndTsuyoPos.Left
    
      Given tobj
      |> When rotateR
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Up
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum/2-1)
      |> Verify

  module 第2つよを左回転 =

    [<Scenario>]
    let ``初期状態時の回転はつよが左に移動する``() =
      let tobj = new TsuyoObj(None,None)

      Given tobj
      |> When rotateL
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Left
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum+RowNum/2-2)
      |> Verify

    [<Scenario>]
    let ``第2つよが左にある時の回転はつよが下に移動する``() =
      let tobj = changePos (RowNum+RowNum/2-2) SndTsuyoPos.Left

      Given tobj
      |> When rotateL
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Down
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2+2)
      |> Verify

    [<Scenario>]
    let ``第2つよが下にある時の左回転はつよが右に移動する``() =
      let tobj = changePos (RowNum*2+2) SndTsuyoPos.Down
    
      Given tobj
      |> When rotateL
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Right
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum+RowNum/2)
      |> Verify

    [<Scenario>]
    let ``第2つよが右にある時の回転はつよが最初の位置に戻る``() =
      let tobj = changePos (RowNum+RowNum/2) SndTsuyoPos.Right
    
      Given tobj
      |> When rotateL
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Up
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum/2-1)
      |> Verify
  
module 障害物がある =

  module 第2つよを右回転 =

    [<Scenario>]
    let ``右壁隣接時の右回転は第1つよが左に、第2つよが第1つよの右に移動する``() =
      let tobj = changePos (RowNum-1) SndTsuyoPos.Up
      tobj.Tsuyo1.Pos <- RowNum*2-1

      Given tobj
      |> When rotateR
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum*2-2)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Right
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2-1)
      |> Verify

    [<Scenario>]
    let ``第1つよの右側が壁で左側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (RowNum-1) SndTsuyoPos.Up
      tobj.Tsuyo1.Pos <- RowNum*2-1
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum*2-2,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateR
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum*2-1)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Up
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum-1)
      |> Verify

    [<Scenario>]
    let ``第2つよの左側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (RowNum*3-1) SndTsuyoPos.Down
      tobj.Tsuyo1.Pos <- RowNum*2-1
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum*3-2,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateR
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum*2-1)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Down
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*3-1)
      |> Verify

    [<Scenario>]
    let ``左壁隣接時の右回転は第1つよが第2つよの右に移動し、第2つよはそのまま``() =
      let tobj = changePos (RowNum*2) SndTsuyoPos.Down
      tobj.Tsuyo1.Pos <- RowNum

      Given tobj
      |> When rotateR
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum*2+1)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Left
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2)
      |> Verify

    [<Scenario>]
    let ``第2つよの左側が壁で右側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (RowNum*2) SndTsuyoPos.Down
      tobj.Tsuyo1.Pos <- RowNum
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum*2+1,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateR
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Down
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2)
      |> Verify

    [<Scenario>]
    let ``第1つよの右側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (RowNum-2) SndTsuyoPos.Up
      tobj.Tsuyo1.Pos <- RowNum*2-2
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum*2-1,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateR
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum*2-2)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Up
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum-2)
      |> Verify

  module 第2つよを左回転 =

    [<Scenario>]
    let ``左壁隣接時の左回転は第1つよが右に、第2つよが第1つよの左に移動する``() =
      let tobj = changePos (0) SndTsuyoPos.Up
      tobj.Tsuyo1.Pos <- RowNum

      Given tobj
      |> When rotateL
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum+1)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Left
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum)
      |> Verify

    [<Scenario>]
    let ``第1つよの左側が壁で右側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (0) SndTsuyoPos.Up
      tobj.Tsuyo1.Pos <- RowNum
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum+1,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateL
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Up
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (0)
      |> Verify

    [<Scenario>]
    let ``第2つよの右側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (RowNum*2) SndTsuyoPos.Down
      tobj.Tsuyo1.Pos <- RowNum
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum*2+1,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateL
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Down
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2)
      |> Verify

    [<Scenario>]
    let ``右壁隣接時の回転は第1つよが第2つよの左に移動し、第2つよはそのまま``() =
      let tobj = changePos (RowNum*2-1) SndTsuyoPos.Down
      tobj.Tsuyo1.Pos <- RowNum-1

      Given tobj
      |> When rotateL
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum*2-2)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Right
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2-1)
      |> Verify

    [<Scenario>]
    let ``第2つよの右側が壁で左側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (RowNum*2-1) SndTsuyoPos.Down
      tobj.Tsuyo1.Pos <- RowNum-1
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum*2-2,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateL
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum-1)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Down
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum*2-1)
      |> Verify

    [<Scenario>]
    let ``第1つよの左側につよが存在する場合、回転できずにそのままの位置を保つ``() =
      let tobj = changePos (RowNum-2) SndTsuyoPos.Up
      tobj.Tsuyo1.Pos <- RowNum*2-2
      TsuyoTsuyo.fieldTsuyo <- [new Tsuyo(RowNum*2-RowNum/2,TsuyoType.Dummy,false)]

      Given tobj
      |> When rotateL
      |> Whereas tobj.Tsuyo1.Pos
      |> It should equal (RowNum*2-2)
      |> Whereas (snd tobj.Tsuyo2)
      |> It should equal SndTsuyoPos.Up
      |> Whereas (fst tobj.Tsuyo2).Pos
      |> It should equal (RowNum-2)
      |> Verify