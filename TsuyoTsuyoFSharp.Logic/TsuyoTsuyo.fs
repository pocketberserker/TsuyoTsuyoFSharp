module TsuyoTsuyo

open Twitterizer
open System.IO
open System.Net
open Microsoft.FSharp.Control.WebExtensions

let RowNum = 6
let ColNum = 13

type TsuyoType =
  | Dummy
  | Real of TwitterStatus

type SndTsuyoPos = | Right | Down | Left | Up

type Tsuyo (pos:int, status:TsuyoType) =

  let mutable position = pos

  let isHidden () =
    if position < RowNum then true
    else false

  let stream () =
    match status with
    | TsuyoType.Real s ->
      async {
        let url = s.User.ProfileImageLocation
        let req = WebRequest.Create url
        let! rsp = req.AsyncGetResponse()

        use stream = rsp.GetResponseStream()
        use read = new BufferedStream(stream)
        let! byteData = read.AsyncRead(int rsp.ContentLength)
        return new MemoryStream(byteData) :> Stream
      } |> Async.RunSynchronously |> Some
    | TsuyoType.Dummy -> None

  member x.Pos with get() = position and set(p) = position <- p
    
  member x.Hidden = isHidden()

  member x.ImageStream = stream ()

  member x.ScreenName = 
    match status with
    | TsuyoType.Real s -> s.User.ScreenName
    | TsuyoType.Dummy -> "##dummy##"

type TsuyoObj (status1:TwitterStatus option, status2:TwitterStatus option) =

  let createTsuyo status pos hidden =
    match status with
    | Some x -> new Tsuyo(pos, Real x)
    | None -> new Tsuyo(pos, Dummy)

  let tsuyo1 = createTsuyo status1 (RowNum+RowNum/2-1) false
  
  let tsuyo2 = createTsuyo status2 (RowNum/2-1) true

  let calcSndTsuyoPos () =
    if  tsuyo2.Pos - tsuyo1.Pos = 1 then SndTsuyoPos.Right
    elif tsuyo2.Pos - tsuyo1.Pos = -1 then SndTsuyoPos.Left
    elif tsuyo2.Pos - tsuyo1.Pos = RowNum then SndTsuyoPos.Down
    else SndTsuyoPos.Up

  member x.Tsuyo1 = tsuyo1

  member x.Tsuyo2 = tsuyo2

  member x.Tsuyo2Pos = calcSndTsuyoPos ()

let mutable fieldTsuyo:Tsuyo list = []

type Direction = | Right | Left | Down
let move (tobj:TsuyoObj) direction =
  (tobj.Tsuyo1, tobj.Tsuyo2)
  ||> fun (x:Tsuyo) (y:Tsuyo) ->
    match direction with
      | Right -> x.Pos <- x.Pos + 1; y.Pos <- y.Pos + 1
      | Left -> x.Pos <- x.Pos - 1; y.Pos <- y.Pos - 1
      | Down -> x.Pos <- x.Pos + RowNum; y.Pos <- y.Pos + RowNum
  tobj

let (|CollideRightWall|CollideLeftWall|CollideBottom|Other|) (tobj:TsuyoObj) =
  if tobj.Tsuyo1.Pos / RowNum = ColNum - 1 || tobj.Tsuyo2.Pos / RowNum = ColNum - 1 then CollideBottom
  elif tobj.Tsuyo1.Pos % RowNum = RowNum - 1 || tobj.Tsuyo2.Pos % RowNum = RowNum - 1 then CollideRightWall
  elif tobj.Tsuyo1.Pos % RowNum = 0 || tobj.Tsuyo2.Pos % RowNum = 0 then CollideLeftWall
  else Other

let isCollideWall (tobj:TsuyoObj) =
  match tobj with
  | CollideRightWall | CollideLeftWall | CollideBottom -> true
  | Other -> false

let detectCollision expr = List.exists (fun (x:Tsuyo) -> x.Pos = expr) fieldTsuyo

type Rotate = | Right | Left
let avoidance (tobj:TsuyoObj) (pos:SndTsuyoPos) (rot:Rotate) =

  let tsuyo1,tsuyo2 = tobj.Tsuyo1, tobj.Tsuyo2

  let (|Collide|AvoidRight|AvoidLeft|Other|) (pos:SndTsuyoPos) : Choice<unit,unit,unit,unit> =
    match tobj with
    | CollideBottom -> Collide
    | CollideRightWall ->
      match pos , rot with
      | SndTsuyoPos.Up , Rotate.Right -> if detectCollision (tsuyo1.Pos - 1) then Collide else AvoidLeft
      | SndTsuyoPos.Up , Rotate.Left -> if detectCollision (tsuyo1.Pos - 1) then Collide else Other
      | SndTsuyoPos.Down , Rotate.Right -> if detectCollision (tsuyo2.Pos - 1) then Collide else Other
      | SndTsuyoPos.Down , Rotate.Left -> if detectCollision (tsuyo2.Pos - 1) then Collide else AvoidLeft
      | SndTsuyoPos.Left , Rotate.Left
      | SndTsuyoPos.Right , Rotate.Right -> if detectCollision (tsuyo1.Pos + RowNum) || detectCollision (tsuyo2.Pos + RowNum) then Collide else Other
      | _ -> Other
    | CollideLeftWall ->
      match pos , rot with
      | SndTsuyoPos.Up , Rotate.Right -> if detectCollision (tsuyo1.Pos + 1) then Collide else Other
      | SndTsuyoPos.Up , Rotate.Left -> if detectCollision (tsuyo1.Pos + 1) then Collide else AvoidRight
      | SndTsuyoPos.Down , Rotate.Right -> if detectCollision (tsuyo2.Pos + 1)then Collide else AvoidRight
      | SndTsuyoPos.Down , Rotate.Left -> if detectCollision (tsuyo2.Pos + 1) then Collide else Other
      | SndTsuyoPos.Left , Rotate.Left
      | SndTsuyoPos.Right , Rotate.Right -> if detectCollision (tsuyo1.Pos + RowNum) || detectCollision (tsuyo2.Pos + RowNum) then Collide else Other
      | _ -> Other
    | Other ->
      match pos , rot with
      | SndTsuyoPos.Up , Rotate.Right -> if detectCollision (tsuyo1.Pos + 1) then Collide else Other
      | SndTsuyoPos.Up , Rotate.Left -> if detectCollision (tsuyo1.Pos - 1) then Collide else Other
      | SndTsuyoPos.Down , Rotate.Right -> if detectCollision (tsuyo2.Pos - 1) then Collide else Other
      | SndTsuyoPos.Down , Rotate.Left -> if detectCollision (tsuyo2.Pos + 1) then Collide else Other
      | SndTsuyoPos.Left , Rotate.Left
      | SndTsuyoPos.Right , Rotate.Right -> if detectCollision (tsuyo1.Pos + RowNum) || detectCollision (tsuyo2.Pos + RowNum) then Collide else Other
      | _ -> Other
  
  match pos, rot with
  | AvoidLeft, Rotate.Right ->
    move tobj Direction.Left |> ignore
    true
  | AvoidRight, Rotate.Left ->
    move tobj Direction.Right |> ignore
    true
  | AvoidRight, Rotate.Right ->
    tsuyo1.Pos <- tsuyo2.Pos + 1
    true
  | AvoidLeft, Rotate.Left ->
    tsuyo1.Pos <- tsuyo2.Pos - 1
    true
  | Other, _ -> true
  | _ -> false

let rotate f1 f2 f3 f4 rot (tobj:TsuyoObj) =
  let pos = tobj.Tsuyo2Pos // 障害物衝突による座標変更前の第2つよの位置
  if avoidance tobj pos rot then
    match pos with
    | SndTsuyoPos.Right -> tobj.Tsuyo2.Pos <- f1 tobj.Tsuyo1.Pos RowNum
    | SndTsuyoPos.Left -> tobj.Tsuyo2.Pos <- f2 tobj.Tsuyo1.Pos RowNum
    | SndTsuyoPos.Up -> tobj.Tsuyo2.Pos <- f3 tobj.Tsuyo1.Pos 1
    | SndTsuyoPos.Down -> tobj.Tsuyo2.Pos <- f4 tobj.Tsuyo1.Pos 1
  tobj

let rotateR tobj = tobj |> rotate (+) (-) (+) (-) Rotate.Right

let rotateL tobj = tobj |> rotate (-) (+) (-) (+) Rotate.Left

let rec fall (tsuyo:Tsuyo) =
  if tsuyo.Pos / RowNum = ColNum - 1 || detectCollision (tsuyo.Pos+RowNum) then tsuyo
  else tsuyo.Pos <- tsuyo.Pos + RowNum; fall tsuyo

let mutable twitStatusList:TwitterStatus option list = []

let createTsuyoObj =
  function
  | [] -> twitStatusList <- []; new TsuyoObj(None,None)
  | [x] -> twitStatusList <- []; new TsuyoObj(x,None)
  | x::xs -> twitStatusList <- xs |> List.tail; new TsuyoObj(x, xs |> List.head)

let created x = twitStatusList <- Some x :: (twitStatusList |> List.rev) |> List.rev

let start () = TwitStream.start "" created

let getUnion name pos =

  let rec getSetList result list =
    match list with
    | [] -> result
    | x::xs -> getSetList (x::result) (xs |> List.filter (fun y -> x <> y))

  let rec getUnion' n p result = 
    let union =
      fieldTsuyo |> List.filter (fun (t:Tsuyo) -> (t.Pos - 1 = p || t.Pos - RowNum = p || t.Pos + 1 = p || t.Pos + RowNum = p) && t.ScreenName = n)
    match union |> List.filter (fun x -> result |> List.forall (fun (y:Tsuyo) -> x.Pos <> y.Pos)) with
    | [] -> result |> getSetList []
    | u -> u |> List.map (fun t -> getUnion' t.ScreenName t.Pos (u |> List.append result)) |> List.concat |> getSetList [];

  getUnion' name pos []