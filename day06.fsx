open System;

let rec transpose list =
    match list with
    | (_::_)::_ ->
        List.map List.head list :: transpose (List.map List.tail list)
    | _ -> []

let break_down list = 
    list
    |> Seq.countBy id
    |> Seq.sortBy (fun (_ ,v) -> v)
    |> Seq.map (fun (k, _) -> k)

let rec get_most_common list =
    list |> break_down |> Seq.head |> string

let rec get_least_common list =
    list |> break_down |> Seq.rev |> Seq.head |> string

let rec solve arr f = 
    arr
    |> Seq.map Seq.toList
    |> Seq.toList
    |> transpose
    |> Seq.map f 
    |> String.concat ""

let input = IO.File.ReadAllLines("./day06.txt")

solve input get_most_common  |> printfn "%s"
solve input get_least_common |> printfn "%s"
