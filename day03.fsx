open System

let input = IO.File.ReadAllLines("./day03.txt")

let get_triples (str:string) =
    (str.[2..4] |> Int32.Parse, str.[7..9] |> Int32.Parse, str.[12..14] |> Int32.Parse)

let is_valid (a, b, c) =
    if (a + b) > c && (a + c) > b && (b + c) > a then 1
    else 0

let list_process list =
    List.sum [ for i in list -> is_valid (get_triples i) ]

let rec list_process_vertical list =
    List.sum [
        match list with
        | [] -> ()
        | a::b::c::rest ->
            let (t1, u1, v1) = get_triples a
            let (t2, u2, v2) = get_triples b
            let (t3, u3, v3) = get_triples c
            yield is_valid (t1, t2, t3) + is_valid (u1, u2, u3) + is_valid (v1, v2, v3)
            yield list_process_vertical rest
        | _ -> failwith "not enough elements left"
    ]

list_process (input |> Array.toList) |> printfn "%i"
list_process_vertical (input |> Array.toList) |> printfn "%i"
