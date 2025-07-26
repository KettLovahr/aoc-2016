open System
open System.Text.RegularExpressions

let exp = Regex "\((\d+)x(\d+)\)"

let rec consume value str =
    match str with
    | "" -> value
    | _ ->
        match str.[0] with
        | '(' ->
            let marker = exp.Match str
            let marker_length =
                (String.length marker.Groups.[0].Value)
            let (chars, rpts) = int marker.Groups.[1].Value,
                                int marker.Groups.[2].Value
            let size = chars * rpts
            let start_index =
                marker_length + chars
            str.[start_index..] |> consume (value + size)
        | _ -> str.[1..] |> consume (value+1)

let rec consume2 (value:int64) str =
    match str with
    | "" -> value
    | _ ->
        match str.[0] with
        | '(' ->
            let marker = exp.Match str
            let marker_length =
                (String.length marker.Groups.[0].Value)
            let (chars, rpts) = int marker.Groups.[1].Value,
                                int marker.Groups.[2].Value
            let end_index =
                marker_length + chars
            let size =
                (int64 rpts) * (consume2 0L str.[marker_length..end_index-1])
            str.[end_index..] |> consume2 (value + size)
        | _ -> str.[1..] |> consume2 (value+1L)

let input = System.IO.File.ReadAllText "./day09.txt"

input.Trim() |> consume 0 |> printfn "%i"
input.Trim() |> consume2 0L |> printfn "%i"
