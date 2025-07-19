open System;

type Action = | Up | Down | Left | Right | Press

let dir_from_char char = match char with
                         | 'U' -> Action.Up
                         | 'D' -> Action.Down
                         | 'L' -> Action.Left
                         | 'R' -> Action.Right
                         | '\n' -> Action.Press
                         | _ -> failwith "Invalid"

let change_button cur dir = match dir with
                            | Up    -> if cur > 3      then cur - 3 else cur
                            | Down  -> if cur < 7      then cur + 3 else cur
                            | Left  -> if cur % 3 <> 1 then cur - 1 else cur
                            | Right -> if cur % 3 <> 0 then cur + 1 else cur
                            | Press -> cur

let rec input_password list button password =
    match list with
    | [] -> String.concat "" password
    | first::rest -> let action = dir_from_char first
                     let new_btn = change_button button action
                     if action = Action.Press then 
                         let new_password = Seq.append password [new_btn |> string]
                         input_password rest new_btn new_password
                     else
                         input_password rest new_btn password

let input = IO.File.ReadAllText("./day02.txt") |> Seq.toList

input_password input 5 []
|> printfn "%s"
