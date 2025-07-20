open System

let input = IO.File.ReadAllLines("./day03.txt")

let get_triples (str:string) =
    (str.[2..4] |> Int32.Parse, str.[7..9] |> Int32.Parse, str.[12..14] |> Int32.Parse)

let is_valid (a, b, c) = (a + b) > c && (a + c) > b && (b + c) > a

let rec list_process list result =
    match list with
    | [] -> result
    | first::rest -> if is_valid (get_triples first) then
                         list_process rest (result + 1)
                     else
                         list_process rest result

let rec list_process_vertical list result =
    match list with
    | [] -> result
    | a::b::c::rest ->
        let ((t1, u1, v1),(t2, u2, v2),(t3, u3, v3)) = get_triples a, get_triples b, get_triples c
        let t = if is_valid (t1, t2, t3) then 1 else 0
        let u = if is_valid (u1, u2, u3) then 1 else 0
        let v = if is_valid (v1, v2, v3) then 1 else 0
        list_process_vertical rest result + t + u + v
    | _ -> failwith "not enough elements left"

list_process (input |> Array.toList) 0 |> printfn "%i"
list_process_vertical (input |> Array.toList) 0 |> printfn "%i"
