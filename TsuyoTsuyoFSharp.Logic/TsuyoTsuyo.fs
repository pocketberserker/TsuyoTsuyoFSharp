module TsuyoTsuyo

open Twitterizer

let RowNum = 6
let ColmunNum = 13

type TsuyoType =
  | Dummy
  | Real of TwitterStatus

type Tsuyo (pos:int, status:TsuyoType, hid:bool) =

  let mutable position = pos

  let mutable hidden = hid

  member x.Pos
    with get() = position
    and set(p) = position <- p
    
  member x.Hidden with get() = hidden and set(h) = hidden <- h 

type TsuyoObj (status1:Option<TwitterStatus>, status2:Option<TwitterStatus>) =

  let createTsuyo status pos hidden =
    match status with
      | Some x -> new Tsuyo(pos, Real x, hidden)
      | None -> new Tsuyo(pos, Dummy, hidden)

  let tsuyo1 = createTsuyo status1 (RowNum+RowNum/2-1) false
  
  let tsuyo2 = createTsuyo status2 (RowNum/2-1) true

  member x.Tsuyo1 = tsuyo1

  member x.Tsuyo2 = tsuyo2