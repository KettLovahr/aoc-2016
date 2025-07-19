open System;

type Action = | Up | Down | Left | Right | Press

let keypad_one = [|
                       [|'1'; '2'; '3'|];
                       [|'4'; '5'; '6'|];
                       [|'7'; '8'; '9'|];
                   |]
let keypad_one_pos = 1, 1

let keypad_two = [|
                       [|' '; ' '; '1'; ' '; ' '|];
                       [|' '; '2'; '3'; '4'; ' '|];
                       [|'5'; '6'; '7'; '8'; '9'|];
                       [|' '; 'A'; 'B'; 'C'; ' '|];
                       [|' '; ' '; 'D'; ' '; ' '|];
                   |]
let keypad_two_pos = 0, 2

let dir_from_char char = match char with
                         | 'U' -> Action.Up     | 'D' -> Action.Down
                         | 'L' -> Action.Left   | 'R' -> Action.Right
                         | '\n' -> Action.Press | _ -> failwith "Invalid"

let button_at (keypad: char[][]) pos = let (x, y) = pos
                                       match keypad.[y].[x] with
                                       | ' ' -> false | _   -> true

let change_button cur dir (keypad: char[][]) =
    let (x, y) = cur
    match dir with
    | Up    -> if y > 0 && (button_at keypad (x, y - 1)) then
                   (x, y - 1)
               else cur
    | Down  -> if y < (Array.length keypad - 1) && (button_at keypad (x, y + 1)) then
                   (x, y + 1)
               else cur
    | Left  -> if x > 0 && (button_at keypad (x - 1, y)) then
                  (x - 1, y)
               else cur
    | Right -> if x < (Array.length keypad.[y] - 1) && (button_at keypad (x + 1, y)) then
                   (x + 1, y)
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

input_password input keypad_one_pos  keypad_one  [] |> printfn "%s"
input_password input keypad_two_pos keypad_two [] |> printfn "%s"
