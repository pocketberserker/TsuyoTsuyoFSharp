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

  let collide' f = List.exists f fieldTsuyo

  let (|LeftCollide|RightCollide|Other|) =
    function
    | SndTsuyoPos.Up ->
      match tsuyo1.Pos % RowNum = RowNum - 1 with 
      | true ->
        match collide' (fun (x:Tsuyo) -> x.Pos = tsuyo1.Pos - 1) with
        | true -> RightCollide true
        | false -> RightCollide false
      | false ->
        match collide' (fun (x:Tsuyo) -> x.Pos = tsuyo1.Pos + 1) with
        | true -> RightCollide true
        | false -> Other
    | SndTsuyoPos.Down ->
      match tsuyo1.Pos % RowNum = 0 with
      | true ->
        match collide' (fun (x:Tsuyo) -> x.Pos = tsuyo2.Pos + 1) with
        | true -> LeftCollide true
        | false -> LeftCollide false
      | false ->
        match collide' (fun (x:Tsuyo) -> x.Pos = tsuyo2.Pos - 1) with
        | true -> LeftCollide true
        | false -> Other
    | _ -> Other
  
  match pos with
  | RightCollide false ->
    tsuyo1.Pos <- tsuyo1.Pos - 1
    tsuyo2.Pos <- tsuyo2.Pos - 1
    true
  | RightCollide true -> false
  | LeftCollide false ->
    tsuyo1.Pos <- tsuyo2.Pos + 1
    true
  | LeftCollide true -> false
  | Other -> true

let rotateR (tobj:TsuyoObj) =

  let rotate' (tsuyo1:Tsuyo) (tsuyo2:Tsuyo) f (num:int) (pos:SndTsuyoPos) =
    tsuyo2.Pos <- f tsuyo1.Pos num
    tsuyo2, pos

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