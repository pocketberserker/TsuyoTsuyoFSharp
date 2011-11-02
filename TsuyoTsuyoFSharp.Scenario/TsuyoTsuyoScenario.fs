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
        |> And (MultiParam [| RowNum-1; RowNum*2-1; Some [new Tsuyo(RowNum*2-2,TsuyoType.Dummy)]; RowNum*2-1; SndTsuyoPos.Up; RowNum-1 |])
        |> And (MultiParam [| RowNum*3-1; RowNum*2-1; Some [new Tsuyo(RowNum*3-2,TsuyoType.Dummy)]; RowNum*2-1; SndTsuyoPos.Down; RowNum*3-1 |])
        |> And (MultiParam [| RowNum*2; RowNum; None; RowNum*2+1; SndTsuyoPos.Left; RowNum*2 |])
        |> And (MultiParam [| RowNum*2; RowNum; Some [new Tsuyo(RowNum*2+1,TsuyoType.Dummy)]; RowNum; SndTsuyoPos.Down; RowNum*2 |])
        |> And (MultiParam [| RowNum-2; RowNum*2-2; Some [new Tsuyo(RowNum*2-1,TsuyoType.Dummy)]; RowNum*2-2; SndTsuyoPos.Up; RowNum-2 |])
        |> And (MultiParam [| RowNum*2-1; RowNum*2-2; Some [new Tsuyo(RowNum*3-2,TsuyoType.Dummy)]; RowNum*2-2; SndTsuyoPos.Right; RowNum*2-1 |])
        |> And (MultiParam [| RowNum*2-1; RowNum*2-2; Some [new Tsuyo(RowNum*3-1,TsuyoType.Dummy)]; RowNum*2-2; SndTsuyoPos.Right; RowNum*2-1 |])
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
        |> And (MultiParam [| 0; RowNum; Some [new Tsuyo(RowNum+1,TsuyoType.Dummy)]; RowNum; SndTsuyoPos.Up; 0 |])
        |> And (MultiParam [| RowNum*2; RowNum; Some [new Tsuyo(RowNum*2+1,TsuyoType.Dummy)]; RowNum; SndTsuyoPos.Down; RowNum*2 |])
        |> And (MultiParam [| RowNum*2-1; RowNum-1; None; RowNum*2-2; SndTsuyoPos.Right; RowNum*2-1 |])
        |> And (MultiParam [| RowNum*2-1; RowNum-1; Some [new Tsuyo(RowNum*2-2,TsuyoType.Dummy)]; RowNum-1; SndTsuyoPos.Down; RowNum*2-1 |])
        |> And (MultiParam [| RowNum-2; RowNum*2-2; Some [new Tsuyo(RowNum*2-RowNum/2,TsuyoType.Dummy)]; RowNum*2-2; SndTsuyoPos.Up; RowNum-2 |])
        |> And (MultiParam [| RowNum; RowNum+1; Some [new Tsuyo(RowNum*2+1,TsuyoType.Dummy)]; RowNum+1; SndTsuyoPos.Left; RowNum |])
        |> And (MultiParam [| RowNum; RowNum+1; Some [new Tsuyo(RowNum*2,TsuyoType.Dummy)]; RowNum+1; SndTsuyoPos.Left; RowNum |])
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

module 落下 =

  let getPos (t:Tsuyo) = t.Pos

  [<Scenario>]
  let ``つよは床と接地していたら落下しない`` () =
    Given (new Tsuyo(RowNum*ColNum-1,TsuyoType.Dummy))
    |> When fall
    |> getPos
    |> It should equal (RowNum*ColNum-1)
    |> Verify

  [<Scenario>]
  let ``つよは床と離れていたら床まで落下する`` () =
    fieldTsuyo <- []

    Given (new Tsuyo(RowNum*(ColNum-2)-1,TsuyoType.Dummy))
    |> When fall
    |> getPos
    |> It should equal (RowNum*ColNum-1)
    |> Verify

  [<Scenario>]
  let ``下のつよと隣接していたら落下しない`` () =
    fieldTsuyo <- [new Tsuyo(RowNum*ColNum-1,TsuyoType.Dummy)]

    Given (new Tsuyo(RowNum*(ColNum-1)-1,TsuyoType.Dummy))
    |> When fall
    |> getPos
    |> It should equal (RowNum*(ColNum-1)-1)
    |> Verify

  [<Scenario>]
  let ``下のつよと2マス以上離れていたらつよの一つ上まで落下`` () =
    fieldTsuyo <- [new Tsuyo(RowNum*ColNum-1,TsuyoType.Dummy)]

    Given (new Tsuyo(RowNum*(ColNum-3)-1,TsuyoType.Dummy))
    |> When fall
    |> getPos
    |> It should equal (RowNum*(ColNum-1)-1)
    |> Verify

module つよオブジェクト生成 =

  let createTwitterStatus name = 
    let user = new Twitterizer.TwitterUser()
    user.ScreenName <- name
    let status = new Twitterizer.TwitterStatus()
    status.User <- user
    Some status

  let getScreenNames (tobj:TsuyoObj) = tobj.Tsuyo1.ScreenName,tobj.Tsuyo2.ScreenName

  [<Scenario>]
  let ``リストにTwitterStatusオブジェクトがなければダミーを二つ格納したつよオブジェクト`` () =
    twitStatusList <- []

    Given twitStatusList
    |> When createTsuyoObj
    |> When getScreenNames
    |> It should equal ("##dummy##","##dummy##")
    |> Whereas twitStatusList
    |> It should equal []
    |> Verify

  [<Scenario>]
  let ``リストにTwitterStatusオブジェクトが一つあれば本物とダミーを格納したつよオブジェクト`` () =
    twitStatusList <- [createTwitterStatus "test"]

    Given twitStatusList
    |> When createTsuyoObj
    |> When getScreenNames
    |> It should equal ("test","##dummy##")
    |> Whereas twitStatusList
    |> It should equal []
    |> Verify

  [<Scenario>]
  let ``リストにTwitterStatusオブジェクトが2つあれば本物2つを格納したつよオブジェクト`` () =
    twitStatusList <- [createTwitterStatus "alice";createTwitterStatus "bob"]

    Given twitStatusList
    |> When createTsuyoObj
    |> When getScreenNames
    |> It should equal ("alice","bob")
    |> Whereas twitStatusList
    |> It should equal []
    |> Verify

  [<Scenario>]
  let ``リストにTwitterStatusオブジェクトが3つあればTwitterStatusオブジェクトが一つ残る`` () =
    let charlie = createTwitterStatus "charlie"
    twitStatusList <- [createTwitterStatus "alice";createTwitterStatus "bob";charlie]

    Given twitStatusList
    |> When createTsuyoObj
    |> Whereas twitStatusList
    |> It should equal [charlie]
    |> Verify

module 連結つよ取得 =

  [<Scenario>]
  let ``同じscreennameのつよが存在しないなら連結は空である``() =
    fieldTsuyo <- []

    Given ("##dummy##",0)
    ||> When getUnion
    |> It should equal []
    |> Verify

  [<Scenario>]
  let ``同じscreennameつよ周りにがあるなら連結は周りにあるつよのリスト``() =
    let tsuyo1 = new Tsuyo(RowNum+2,TsuyoType.Dummy)
    let tsuyo2 = new Tsuyo(RowNum*2+1,TsuyoType.Dummy)
    let tsuyo3 = new Tsuyo(RowNum,TsuyoType.Dummy)
    let tsuyo4 = new Tsuyo(1,TsuyoType.Dummy)
    fieldTsuyo <- [tsuyo1; tsuyo2; tsuyo3; tsuyo4]

    Given ("##dummy##",RowNum+1)
    ||> When getUnion
    |> It should contain tsuyo1
    |> It should contain tsuyo2
    |> It should contain tsuyo3
    |> It should contain tsuyo4
    |> It should have (length 4)
    |> Verify

  [<Scenario>]
  let ``連結していると判定されたつよの周りに同じscreennameつよが存在すればそのつよも連結結果にいれる``() =
    let tsuyo1 = new Tsuyo(RowNum+2,TsuyoType.Dummy)
    let tsuyo2 = new Tsuyo(RowNum*2+1,TsuyoType.Dummy)
    let tsuyo3 = new Tsuyo(RowNum+3,TsuyoType.Dummy)
    let tsuyo4 = new Tsuyo(RowNum*2+2,TsuyoType.Dummy)
    fieldTsuyo <- [tsuyo1; tsuyo2; tsuyo3; tsuyo4]

    Given ("##dummy##",RowNum+1)
    ||> When getUnion
    |> It should contain tsuyo1
    |> It should contain tsuyo2
    |> It should contain tsuyo3
    |> It should contain tsuyo4
    |> It should have (length 4)
    |> Verify


  [<Scenario>]
  let ``連結していないつよはリストに含まれない``() =
    let tsuyo1 = new Tsuyo(RowNum*(ColNum-1),TsuyoType.Dummy)
    let tsuyo2 = new Tsuyo(RowNum*(ColNum-2),TsuyoType.Dummy)
    let tsuyo3 = new Tsuyo(RowNum*ColNum-1,TsuyoType.Dummy)
    fieldTsuyo <- [tsuyo1; tsuyo2; tsuyo3]

    Given ("##dummy##",RowNum*(ColNum-1)-1)
    ||> When getUnion
    |> It should equal [tsuyo3]
    |> It should have (length 1)
    |> Verify

  [<Scenario>]
  let ``screemnameの違うつよはつよはリストに含まれない``() =
    let tsuyo1 = new Tsuyo(RowNum+2,TsuyoType.Dummy)
    let tsuyo2 = new Tsuyo(RowNum+3,TsuyoType.Dummy)
    let user = new Twitterizer.TwitterUser()
    user.ScreenName <- "testname"
    let status = new Twitterizer.TwitterStatus()
    status.User <- user
    let tsuyo3 = new Tsuyo(RowNum+4,TsuyoType.Real status)
    fieldTsuyo <- [tsuyo1; tsuyo2; tsuyo3]

    Given ("##dummy##",RowNum+1)
    ||> When getUnion
    |> It should contain tsuyo1
    |> It should contain tsuyo2
    |> It should have (length 2)
    |> Verify

module 消去 =

  [<Scenario>]
  let ``連結が3個以下ならフィールドから削除しない``() =
    let tsuyo1 = new Tsuyo(1,TsuyoType.Dummy)
    let tsuyo2 = new Tsuyo(2,TsuyoType.Dummy)
    let tsuyo3 = new Tsuyo(3,TsuyoType.Dummy)
    fieldTsuyo <- [tsuyo1; tsuyo2; tsuyo3]

    Given ("##dummy##",0)
    ||> When getUnion
    |> When getAfterEraseList
    |> It should contain tsuyo1
    |> It should contain tsuyo2
    |> It should contain tsuyo3
    |> It should have (length 3)
    |> Verify

  [<Scenario>]
  let ``連結が4個以上ならフィールドから削除する``() =
    let tsuyo1 = new Tsuyo(1,TsuyoType.Dummy)
    let tsuyo2 = new Tsuyo(2,TsuyoType.Dummy)
    let tsuyo3 = new Tsuyo(3,TsuyoType.Dummy)
    let tsuyo4 = new Tsuyo(4,TsuyoType.Dummy)
    fieldTsuyo <- [tsuyo1; tsuyo2; tsuyo3; tsuyo4]

    Given ("##dummy##",0)
    ||> When getUnion
    |> When getAfterEraseList
    |> It should equal []
    |> Verify

  [<Scenario>]
  let ``screemnameの違うつよは消えずにフィールドに残る``() =
    let tsuyo1 = new Tsuyo(1,TsuyoType.Dummy)
    let tsuyo2 = new Tsuyo(2,TsuyoType.Dummy)
    let tsuyo3 = new Tsuyo(3,TsuyoType.Dummy)
    let tsuyo4 = new Tsuyo(4,TsuyoType.Dummy)
    let user = new Twitterizer.TwitterUser()
    user.ScreenName <- "testname"
    let status = new Twitterizer.TwitterStatus()
    status.User <- user
    let tsuyo5 = new Tsuyo(4,TsuyoType.Real status)
    fieldTsuyo <- [tsuyo1; tsuyo2; tsuyo3; tsuyo4; tsuyo5]

    Given ("##dummy##",RowNum+1)
    ||> When getUnion
    |> When getAfterEraseList
    |> It should equal [tsuyo5]
    |> Verify