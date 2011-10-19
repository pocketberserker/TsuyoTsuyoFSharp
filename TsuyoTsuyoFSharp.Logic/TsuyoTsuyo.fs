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

let avoidance (tsuyo1:Tsuyo) (tsuyo2:Tsuyo) (pos:SndTsuyoPos) =

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

  let (|RightCollide|LeftCollide|Other|) (pos:SndTsuyoPos) : Choice<bool,bool,unit> =
    match pos with
    | SndTsuyoPos.Up ->
      collide' (RowNum - 1) (tsuyo1.Pos - 1) (tsuyo1.Pos + 1) (RightCollide true) (RightCollide false) Other
    | SndTsuyoPos.Down ->
      collide' 0 (tsuyo2.Pos + 1) (tsuyo2.Pos - 1) (LeftCollide true) (LeftCollide false) Other
    | _ -> Other
  
  match pos with
  | RightCollide false ->
    tsuyo1.Pos <- tsuyo1.Pos - 1
    tsuyo2.Pos <- tsuyo2.Pos - 1
    true
  | LeftCollide false ->
    tsuyo1.Pos <- tsuyo2.Pos + 1
    true
  | LeftCollide true | RightCollide true -> false
  | Other -> true

let rotate' (tsuyo1:Tsuyo) (tsuyo2:Tsuyo) f (num:int) (pos:SndTsuyoPos) =
    tsuyo2.Pos <- f tsuyo1.Pos num
    tsuyo2, pos

let rotateR (tobj:TsuyoObj) =

  match tobj.Tsuyo2 with
  | (tsuyo2, SndTsuyoPos.Right) ->
    tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (+) RowNum SndTsuyoPos.Down
  | (tsuyo2, SndTsuyoPos.Left) ->
    tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (-) RowNum SndTsuyoPos.Up
  | (tsuyo2, pos) ->
    match avoidance tobj.Tsuyo1 tsuyo2 pos with
    | true ->
      match pos with
      | SndTsuyoPos.Up -> tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (+) 1 SndTsuyoPos.Right
      | SndTsuyoPos.Down -> tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (-) 1 SndTsuyoPos.Left
      | _ -> ()
    | false -> ()

let rotateL (tobj:TsuyoObj) =
  match tobj.Tsuyo2 with
  | (tsuyo2, SndTsuyoPos.Up) ->
    tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (-) 1 SndTsuyoPos.Left
  | (tsuyo2, SndTsuyoPos.Left) ->
    tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (+) RowNum SndTsuyoPos.Down
  | (tsuyo2, SndTsuyoPos.Down) ->
    tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (+) 1 SndTsuyoPos.Right
  | (tsuyo2, SndTsuyoPos.Right) ->
    tobj.Tsuyo2 <- rotate' tobj.Tsuyo1 tsuyo2 (-) RowNum SndTsuyoPos.Up