open System;

type Action = | Up | Down | Left | Right | Press

let first_keypad = [|
                       [|'1'; '2'; '3'|];
                       [|'4'; '5'; '6'|];
                       [|'7'; '8'; '9'|];
                   |]
let first_keypad_position = 1, 1

let second_keypad = [|
                       [|' '; ' '; '1'; ' '; ' '|];
                       [|' '; '2'; '3'; '4'; ' '|];
                       [|'5'; '6'; '7'; '8'; '9'|];
                       [|' '; 'A'; 'B'; 'C'; ' '|];
                       [|' '; ' '; 'D'; ' '; ' '|];
                   |]
let second_keypad_position = 0, 2

let dir_from_char char = match char with
                         | 'U' -> Action.Up     | 'D' -> Action.Down
                         | 'L' -> Action.Left   | 'R' -> Action.Right
                         | '\n' -> Action.Press | _ -> failwith "Invalid"

let change_button cur dir (keypad: char[][]) =
    let (x, y) = cur
    match dir with
    | Up    -> if y > 0 && keypad.[y - 1].[x] <> ' ' then (x, y - 1)
               else cur
    | Down  -> if y < (Array.length keypad - 1) && keypad.[y + 1].[x] <> ' ' then (x, y + 1)
               else cur
    | Left  -> if x > 0 && keypad.[y].[x - 1] <> ' ' then (x - 1, y)
               else cur
    | Right -> if x < (Array.length keypad.[y] - 1) && keypad.[y].[x + 1] <> ' ' then (x + 1, y)
               else cur
    | Press -> cur

let rec input_password list button keypad password =
    match list with
    | [] -> String.concat "" password
    | first::rest -> let action = dir_from_char first
                     let new_btn = change_button button action keypad
                     if action = Action.Press then
                         let (x, y) = new_btn
                         let new_password = Seq.append password [keypad.[y].[x] |> string]
                         input_password rest new_btn keypad new_password
                     else
                         input_password rest new_btn keypad password

let input = IO.File.ReadAllText("./day02.txt") |> Seq.toList

input_password input first_keypad_position  first_keypad  [] |> printfn "%s"
input_password input second_keypad_position second_keypad [] |> printfn "%s"
