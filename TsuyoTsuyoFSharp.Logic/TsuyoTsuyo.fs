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

let rotateR (tobj:TsuyoObj) = 
  match tobj.Tsuyo2 with
    | (tsuyo, SndTsuyoPos.Right) ->
      tsuyo.Pos <- tobj.Tsuyo1.Pos + RowNum
      tobj.Tsuyo2 <- tsuyo, SndTsuyoPos.Down
    | (tsuyo, SndTsuyoPos.Down) ->
      tsuyo.Pos <- tobj.Tsuyo1.Pos - 1
      tobj.Tsuyo2 <- tsuyo, SndTsuyoPos.Left
    | (tsuyo, SndTsuyoPos.Left) ->
      tsuyo.Pos <- tobj.Tsuyo1.Pos - RowNum
      tobj.Tsuyo2 <- tsuyo, SndTsuyoPos.Up
    | (tsuyo, SndTsuyoPos.Up) ->
      tsuyo.Pos <- tobj.Tsuyo1.Pos + 1
      tobj.Tsuyo2 <- tsuyo, SndTsuyoPos.Right

