open System
open System.Text.RegularExpressions

type Instruction =
| Rect of (int * int)
| RotateRow of (int * int)
| RotateColumn of (int * int)
| NoOp

let get_instruction (str:string): Instruction =
    let rect = Regex("rect (\d+)x(\d+)")
    let rot_row = Regex("rotate row y=(\d+) by (\d+)")
    let rot_col = Regex("rotate column x=(\d+) by (\d+)")
    if rect.IsMatch(str) then
        let m = rect.Match(str)
        Rect (m.Groups.[1].Value |> Int32.Parse, m.Groups.[2].Value |> Int32.Parse)
    elif rot_row.IsMatch(str) then
        let m = rot_row.Match(str)
        RotateRow(m.Groups.[1].Value |> Int32.Parse, m.Groups.[2].Value |> Int32.Parse)
    elif rot_col.IsMatch(str) then
        let m = rot_col.Match(str)
        RotateColumn(m.Groups.[1].Value |> Int32.Parse, m.Groups.[2].Value |> Int32.Parse)
    else
        NoOp

let emod a b = ((a % b) + b) % b

let rec run_operation (list:bool array array) (insts:Instruction list) = 
    match insts with
    // i'm so fucking sorry
    | inst::tail ->
        match inst with
        | Rect (w, h) ->
            let new_list =
                list
                |> Array.mapi (fun y v ->
                               if h > y then
                                   v |> Array.mapi (fun x v -> if w > x then true else v)
                               else v)
            run_operation new_list tail
        | RotateRow (row, amt) ->
            let new_list =
                list
                |> Array.mapi (fun y v ->
                               if row = y then
                                   v |> Array.mapi (fun x _ -> v.[emod (x - amt) (Array.length v)])
                               else v)
            run_operation new_list tail
        | RotateColumn (col, amt) ->
            let new_list =
                list
                |> Array.mapi (fun y v ->
                               v |> Array.mapi (fun x w ->
                                                if col = x then
                                                    list.[emod (y - amt) (Array.length list)].[x]
                                                else w))
            run_operation new_list tail
        | _ -> run_operation list tail
    | [] -> list

let list_to_string (list:bool array array) =
    list |> Seq.map (fun x -> x |> Seq.map (fun y -> if y then "#" else " ") |> String.concat "")
         |> String.concat "\n"

let count_lit (list:bool array array) =
    list |> Seq.map (fun x -> x |> Seq.map (fun y -> if y then 1 else 0) |> Seq.sum)
         |> Seq.sum
    
let width = 50
let height = 6
let screen = Array.init height (fun _ -> Array.init width (fun _ -> false))

let input = IO.File.ReadAllLines("./day08.txt")

run_operation screen (input |> Seq.map get_instruction |> Seq.toList) |> count_lit |> printfn "%i"
run_operation screen (input |> Seq.map get_instruction |> Seq.toList) |> list_to_string |> printfn "%s"
