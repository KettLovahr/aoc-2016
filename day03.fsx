open System

let input = IO.File.ReadAllLines("./day03.txt")

let get_triples (str:string) =
    (str.[2..4] |> Int32.Parse, str.[7..9] |> Int32.Parse, str.[12..14] |> Int32.Parse)

let count (a, b, c) =
    if (a + b) > c && (a + c) > b && (b + c) > a then 1
    else 0

let rec list_process list result =
    match list with
    | [] -> result
    | first::rest -> result + count (get_triples first)
                     |> list_process rest

let rec list_process_vertical list result =
    match list with
    | [] -> result
    | a::b::c::rest ->
        let ((t1, u1, v1),(t2, u2, v2),(t3, u3, v3)) =
            get_triples a, get_triples b, get_triples c
        result + count(t1,t2,t3) + count(u1,u2,u3) + count(v1,v2,v3)
        |> list_process_vertical rest
    | _ -> failwith "not enough elements left"

list_process (input |> Array.toList) 0 |> printfn "%i"
list_process_vertical (input |> Array.toList) 0 |> printfn "%i"
