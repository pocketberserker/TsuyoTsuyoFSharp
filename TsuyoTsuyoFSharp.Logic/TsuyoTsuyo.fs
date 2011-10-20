module TsuyoTsuyo

open Twitterizer

let RowNum = 6
let ColmunNum = 13

type TsuyoType =
  | Dummy
  | Real of TwitterStatus

type SndTsuyoPos = | Right | Down | Left | Up

type Tsuyo (pos:int, status:TsuyoType, hid:bool) =

  let mutable position = pos

  let mutable hidden = hid

  member x.Pos
    with get() = position
    and set(p) = position <- p
    
  member x.Hidden with get() = hidden and set(h) = hidden <- h 

type TsuyoObj (status1:TwitterStatus option, status2:TwitterStatus option) =

  let createTsuyo status pos hidden =
    match status with
    | Some x -> new Tsuyo(pos, Real x, hidden)
    | None -> new Tsuyo(pos, Dummy, hidden)

  let tsuyo1 = createTsuyo status1 (RowNum+RowNum/2-1) false
  
  let mutable tsuyo2 = createTsuyo status2 (RowNum/2-1) true , SndTsuyoPos.Up

  member x.Tsuyo1 = tsuyo1

  member x.Tsuyo2 with get() = tsuyo2 and set(t) = tsuyo2 <- t

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

  // trueで衝突して回転不可能、falseで衝突するが回転可能
  let (|RightCollide|LeftCollide|BottomCollide|Other|) (pos:SndTsuyoPos) : Choice<bool,bool,unit,unit> =
    match pos with
    | SndTsuyoPos.Up ->
      match rot with
      | Rotate.Right -> collide' (RowNum - 1) (tsuyo1.Pos - 1) (tsuyo1.Pos + 1) (RightCollide true) (RightCollide false) Other
      | Rotate.Left -> collide' 0 (tsuyo1.Pos + 1) (tsuyo1.Pos - 1) (LeftCollide true) (LeftCollide false) Other
    | SndTsuyoPos.Down ->
      match rot with
      | Rotate.Right -> collide' 0 (tsuyo2.Pos + 1) (tsuyo2.Pos - 1) (LeftCollide true) (LeftCollide false) Other
      | Rotate.Left -> collide' (RowNum - 1) (tsuyo2.Pos - 1) (tsuyo2.Pos + 1) (RightCollide true) (RightCollide false) Other
    | SndTsuyoPos.Left ->
      match rot with
      | Rotate.Left -> collide'' BottomCollide Other
      | _ -> Other
    | SndTsuyoPos.Right ->
      match rot with
      | Rotate.Right -> collide'' BottomCollide Other
      | _ -> Other
  
  match pos, rot with
  | (RightCollide false), Rotate.Right ->
    tsuyo1.Pos <- tsuyo1.Pos - 1
    tsuyo2.Pos <- tsuyo2.Pos - 1
    true
  | (LeftCollide false), Rotate.Left ->
    tsuyo1.Pos <- tsuyo1.Pos + 1
    tsuyo2.Pos <- tsuyo2.Pos + 1
    true
  | LeftCollide false, Rotate.Right ->
    tsuyo1.Pos <- tsuyo2.Pos + 1
    true
  | RightCollide false, Rotate.Left ->
    tsuyo1.Pos <- tsuyo2.Pos - 1
    true
  | Other, _ -> true
  | _ -> false

let rotate (tobj:TsuyoObj) f1 pos1 f2 pos2 f3 pos3 f4 pos4 rot =

  let rotate' (tsuyo1:Tsuyo) (tsuyo2:Tsuyo) f (num:int) (pos:SndTsuyoPos) =
    tsuyo2.Pos <- f tsuyo1.Pos num
    tsuyo2, pos
  
  let tsuyo2 = fst tobj.Tsuyo2
  let pos = snd tobj.Tsuyo2
  match avoidance tobj.Tsuyo1 tsuyo2 pos rot with
  | true ->
    match pos with
    | SndTsuyoPos.Right -> tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 f1 RowNum pos1
    | SndTsuyoPos.Left -> tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 f2 RowNum pos2
    | SndTsuyoPos.Up -> tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 f3 1 pos3
    | SndTsuyoPos.Down -> tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 f4 1 pos4
  | false -> ()

let rotateR tobj = rotate tobj (+) SndTsuyoPos.Down (-) SndTsuyoPos.Up (+) SndTsuyoPos.Right (-) SndTsuyoPos.Left Rotate.Right

let rotateL tobj = rotate tobj (-) SndTsuyoPos.Up (+) SndTsuyoPos.Down (-) SndTsuyoPos.Left (+) SndTsuyoPos.Right Rotate.Left