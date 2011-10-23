module TsuyoTsuyo

open Twitterizer
open System.IO
open System.Net
open Microsoft.FSharp.Control.WebExtensions

let RowNum = 6
let ColmunNum = 13

type TsuyoType =
  | Dummy
  | Real of TwitterStatus

type SndTsuyoPos = | Right | Down | Left | Up

type Tsuyo (pos:int, status:TsuyoType, hid:bool) =

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

        return rsp.GetResponseStream()
      } |> Async.RunSynchronously
    | TsuyoType.Dummy -> File.OpenRead(@"480_16colors_normal.png") :> Stream

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
    | Some x -> new Tsuyo(pos, Real x, hidden)
    | None -> new Tsuyo(pos, Dummy, hidden)

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

type Rotate = | Right | Left
let avoidance (tsuyo1:Tsuyo) (tsuyo2:Tsuyo) (pos:SndTsuyoPos) (rot:Rotate) =

  let collide' num expr1 expr2 pattarn1 pattarn2 other = 
    match tsuyo1.Pos % RowNum = num with 
    | true ->
      match List.exists (fun (x:Tsuyo) -> x.Pos = expr1) fieldTsuyo with
      | true -> pattarn1
      | false -> pattarn2
    | false ->
      match List.exists (fun (x:Tsuyo) -> x.Pos = expr2) fieldTsuyo with
      | true -> pattarn1
      | false -> other

  let collide'' pattarn other =
    match List.exists (fun (x:Tsuyo) -> x.Pos = tsuyo1.Pos + RowNum) fieldTsuyo with
    | true -> pattarn
    | false ->
      match List.exists (fun (x:Tsuyo) -> x.Pos = tsuyo2.Pos + RowNum) fieldTsuyo with
      | true -> pattarn
      | false -> other

  let (|CollideRight|CollideLeft|CollideBottom|AvoidRight|AvoidLeft|Other|) (pos:SndTsuyoPos) : Choice<unit,unit,unit,unit,unit,unit> =
    match pos , rot with
    | SndTsuyoPos.Up , Rotate.Right -> collide' (RowNum - 1) (tsuyo1.Pos - 1) (tsuyo1.Pos + 1) CollideRight AvoidLeft Other
    | SndTsuyoPos.Up , Rotate.Left -> collide' 0 (tsuyo1.Pos + 1) (tsuyo1.Pos - 1) CollideLeft AvoidRight Other
    | SndTsuyoPos.Down , Rotate.Right -> collide' 0 (tsuyo2.Pos + 1) (tsuyo2.Pos - 1) CollideLeft AvoidRight Other
    | SndTsuyoPos.Down , Rotate.Left -> collide' (RowNum - 1) (tsuyo2.Pos - 1) (tsuyo2.Pos + 1) CollideRight AvoidLeft Other
    | SndTsuyoPos.Left , Rotate.Left
    | SndTsuyoPos.Right , Rotate.Right -> collide'' CollideBottom Other
    | _ -> Other
  
  match pos, rot with
  | AvoidLeft, Rotate.Right ->
    tsuyo1.Pos <- tsuyo1.Pos - 1
    tsuyo2.Pos <- tsuyo2.Pos - 1
    true
  | AvoidRight, Rotate.Left ->
    tsuyo1.Pos <- tsuyo1.Pos + 1
    tsuyo2.Pos <- tsuyo2.Pos + 1
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
  if avoidance tobj.Tsuyo1 tobj.Tsuyo2 pos rot then
    match pos with
    | SndTsuyoPos.Right -> tobj.Tsuyo2.Pos <- f1 tobj.Tsuyo1.Pos RowNum
    | SndTsuyoPos.Left -> tobj.Tsuyo2.Pos <- f2 tobj.Tsuyo1.Pos RowNum
    | SndTsuyoPos.Up -> tobj.Tsuyo2.Pos <- f3 tobj.Tsuyo1.Pos 1
    | SndTsuyoPos.Down -> tobj.Tsuyo2.Pos <- f4 tobj.Tsuyo1.Pos 1
  tobj

let rotateR tobj = tobj |> rotate (+) (-) (+) (-) Rotate.Right

let rotateL tobj = tobj |> rotate (-) (+) (-) (+) Rotate.Left
