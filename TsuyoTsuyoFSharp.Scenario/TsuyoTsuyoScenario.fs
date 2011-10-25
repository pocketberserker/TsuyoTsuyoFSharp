module TsuyoTsuyoScenario

open NUnit.Framework
open NaturalSpec
open TsuyoTsuyo

let changePos t2pos t1pos list =
  let tobj = new TsuyoObj(None,None)
  tobj.Tsuyo2.Pos <- t2pos
  tobj.Tsuyo1.Pos <- t1pos
  match list with
  | Some x -> TsuyoTsuyo.fieldTsuyo <- x
  | None -> TsuyoTsuyo.fieldTsuyo <- []
  tobj

let getTsuyo1Pos (tobj:TsuyoObj) = tobj.Tsuyo1.Pos

let getTsuyo2Pos (tobj:TsuyoObj) = tobj.Tsuyo2Pos, tobj.Tsuyo2.Pos

module 周りに障害物がない =

  module 第2つよを右回転 =

    let RightTestCases =
      TestWith (tripleParam (RowNum/2-1) SndTsuyoPos.Right (RowNum+RowNum/2))
        |> And (tripleParam (RowNum+RowNum/2) SndTsuyoPos.Down (RowNum*2+2))
        |> And (tripleParam (RowNum*2+2) SndTsuyoPos.Left (RowNum+RowNum/2-2))
        |> And (tripleParam (RowNum+RowNum/2-2) SndTsuyoPos.Up (RowNum/2-1))
    
    [<Scenario>]
    [<ScenarioSource "RightTestCases">]
    let ``第2つよt2pos,第1つよ初期位置時の回転結果`` t2pos result1 result2 =
      let tobj = changePos t2pos (RowNum+RowNum/2-1) None
    
      Given tobj
      |> When rotateR
      |> Whereas tobj.Tsuyo2Pos
      |> It should equal result1
      |> Whereas tobj.Tsuyo2.Pos
      |> It should equal result2
      |> Verify

  module 第2つよを左回転 =

    let LeftTestCases =
      TestWith (tripleParam (RowNum/2-1) SndTsuyoPos.Left (RowNum+RowNum/2-2))
        |> And (tripleParam (RowNum+RowNum/2-2) SndTsuyoPos.Down (RowNum*2+2))
        |> And (tripleParam (RowNum*2+2) SndTsuyoPos.Right (RowNum+RowNum/2))
        |> And (tripleParam (RowNum+RowNum/2) SndTsuyoPos.Up (RowNum/2-1))

    [<Scenario>]
    [<ScenarioSource "LeftTestCases">]
    let ``第2つよt2pos,第1つよ初期位置時の回転結果`` t2pos result1 result2 =
      let tobj = changePos t2pos (RowNum+RowNum/2-1) None
    
      Given tobj
      |> When rotateL
      |> Whereas tobj.Tsuyo2Pos
      |> It should equal result1
      |> Whereas tobj.Tsuyo2.Pos
      |> It should equal result2
      |> Verify
  
module 障害物がある =

  module 第2つよを右回転 =

    let RightTestCases =
      TestWith (MultiParam [| RowNum-1; RowNum*2-1; None; RowNum*2-2; SndTsuyoPos.Right; RowNum*2-1 |])
        |> And (MultiParam [| RowNum-1; RowNum*2-1; Some [new Tsuyo(RowNum*2-2,TsuyoType.Dummy,false)]; RowNum*2-1; SndTsuyoPos.Up; RowNum-1 |])
        |> And (MultiParam [| RowNum*3-1; RowNum*2-1; Some [new Tsuyo(RowNum*3-2,TsuyoType.Dummy,false)]; RowNum*2-1; SndTsuyoPos.Down; RowNum*3-1 |])
        |> And (MultiParam [| RowNum*2; RowNum; None; RowNum*2+1; SndTsuyoPos.Left; RowNum*2 |])
        |> And (MultiParam [| RowNum*2; RowNum; Some [new Tsuyo(RowNum*2+1,TsuyoType.Dummy,false)]; RowNum; SndTsuyoPos.Down; RowNum*2 |])
        |> And (MultiParam [| RowNum-2; RowNum*2-2; Some [new Tsuyo(RowNum*2-1,TsuyoType.Dummy,false)]; RowNum*2-2; SndTsuyoPos.Up; RowNum-2 |])
        |> And (MultiParam [| RowNum*2-1; RowNum*2-2; Some [new Tsuyo(RowNum*3-2,TsuyoType.Dummy,false)]; RowNum*2-2; SndTsuyoPos.Right; RowNum*2-1 |])
        |> And (MultiParam [| RowNum*2-1; RowNum*2-2; Some [new Tsuyo(RowNum*3-1,TsuyoType.Dummy,false)]; RowNum*2-2; SndTsuyoPos.Right; RowNum*2-1 |])
        |> And (MultiParam [| RowNum*ColNum-1; RowNum*ColNum-2; None; RowNum*ColNum-2; SndTsuyoPos.Right; RowNum*ColNum-1 |])

    [<Scenario>]
    [<ScenarioSource "RightTestCases">]
    let ``第2つよt2pos,第1つよ位置t1pos,フィールド状態list時の右回転結果`` t2pos t1pos list result1 result2 result3 =
      let tobj = changePos t2pos t1pos list 

      Given tobj
      |> When rotateR
      |> When getTsuyo1Pos
      |> It should equal result1
      |> Whereas (getTsuyo2Pos tobj)
      |> It should equal (result2, result3)
      |> Verify

  module 第2つよを左回転 =

    let LeftTestCases =
      TestWith (MultiParam [| 0; RowNum; None; RowNum+1; SndTsuyoPos.Left; RowNum |])
        |> And (MultiParam [| 0; RowNum; Some [new Tsuyo(RowNum+1,TsuyoType.Dummy,false)]; RowNum; SndTsuyoPos.Up; 0 |])
        |> And (MultiParam [| RowNum*2; RowNum; Some [new Tsuyo(RowNum*2+1,TsuyoType.Dummy,false)]; RowNum; SndTsuyoPos.Down; RowNum*2 |])
        |> And (MultiParam [| RowNum*2-1; RowNum-1; None; RowNum*2-2; SndTsuyoPos.Right; RowNum*2-1 |])
        |> And (MultiParam [| RowNum*2-1; RowNum-1; Some [new Tsuyo(RowNum*2-2,TsuyoType.Dummy,false)]; RowNum-1; SndTsuyoPos.Down; RowNum*2-1 |])
        |> And (MultiParam [| RowNum-2; RowNum*2-2; Some [new Tsuyo(RowNum*2-RowNum/2,TsuyoType.Dummy,false)]; RowNum*2-2; SndTsuyoPos.Up; RowNum-2 |])
        |> And (MultiParam [| RowNum; RowNum+1; Some [new Tsuyo(RowNum*2+1,TsuyoType.Dummy,false)]; RowNum+1; SndTsuyoPos.Left; RowNum |])
        |> And (MultiParam [| RowNum; RowNum+1; Some [new Tsuyo(RowNum*2,TsuyoType.Dummy,false)]; RowNum+1; SndTsuyoPos.Left; RowNum |])
        |> And (MultiParam [| RowNum*ColNum-2; RowNum*ColNum-1; None; RowNum*ColNum-1; SndTsuyoPos.Left; RowNum*ColNum-2 |])

    [<Scenario>]
    [<ScenarioSource "LeftTestCases">]
    let ``第2つよt2pos,第1つよ位置t1pos,フィールド状態list時の左回転結果`` t2pos t1pos list result1 result2 result3 =
      let tobj = changePos t2pos t1pos list 

      Given tobj
      |> When rotateL
      |> When getTsuyo1Pos
      |> It should equal result1
      |> Whereas (getTsuyo2Pos tobj)
      |> It should equal (result2, result3)
      |> Verify

module 移動 =

  [<Scenario>]
  let ``右方向なら位置を一つ右に移動する`` () =
   let tobj = changePos 0 RowNum None

   Given (tobj , Direction.Right)
   ||> When move
   |> When getTsuyo1Pos
   |> It should equal (RowNum+1)
   |> Whereas tobj.Tsuyo2.Pos
   |> It should equal 1
   |> Verify

  [<Scenario>]
  let ``左方向なら位置を一つ左に移動する`` () =
   let tobj = changePos 1 (RowNum+1) None

   Given (tobj , Direction.Left)
   ||> When move
   |> When getTsuyo1Pos
   |> It should equal (RowNum)
   |> Whereas tobj.Tsuyo2.Pos
   |> It should equal 0
   |> Verify

  [<Scenario>]
  let ``下方向なら位置を一つ下に移動する`` () =
   let tobj = changePos 0 RowNum None

   Given (tobj , Direction.Down)
   ||> When move
   |> When getTsuyo1Pos
   |> It should equal (RowNum*2)
   |> Whereas tobj.Tsuyo2.Pos
   |> It should equal RowNum
   |> Verify

module 壁の衝突判定 =

  let CollideTestCases =
    TestWith (MultiParam [| RowNum-2; RowNum-1; None; true |])
      |> And (MultiParam [| RowNum+1; RowNum; None; true |])
      |> And (MultiParam [| RowNum*2-1; RowNum-1; None; true |])
      |> And (MultiParam [| RowNum*2; RowNum; None; true |])
      |> And (MultiParam [| RowNum*(ColNum-1)-2; RowNum*ColNum-2; None; true |])
      |> And (MultiParam [| RowNum*ColNum-1; RowNum*ColNum-2; None; true |])
      |> And (MultiParam [| RowNum/2-1; RowNum/2-1; None; false |])
      |> And (MultiParam [| RowNum-2; RowNum-1; None; true |])
      |> And (MultiParam [| RowNum-2; RowNum-1; None; true |])

  [<Scenario>]
  [<ScenarioSource "CollideTestCases">]
  let ``つよが壁に衝突するかの結果`` t2pos t1pos list result =
   let tobj = changePos t2pos t1pos list

   Given tobj
   |> When isCollideWall
   |> It should equal result
   |> Verify