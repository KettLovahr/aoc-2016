open System;

let rec transpose list =
    match list with
    | (_::_)::_ ->
        List.map List.head list :: transpose (List.map List.tail list)
    | _ -> []

let rec break_down (map:Map<char,int>) list = 
    match list with
    | [] -> map
            |> Seq.sortWith (fun a b -> b.Value - a.Value) 
            |> Seq.map (fun a -> a.Key)
    | head::tail -> if Map.containsKey head map then
                        let new_map = map.Add (head, map.[head] + 1)
                        break_down new_map tail
                    else
                        let new_map = map.Add (head, 1)
                        break_down new_map tail

let rec get_most_common list =
    list |> break_down Map.empty |> Seq.head |> string

let rec get_least_common list =
    list |> break_down Map.empty |> Seq.rev |> Seq.head |> string

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
