open System

let rec break_down list map =
    match list with
    | [] -> map
    | head::rest ->
        if Map.containsKey head map then
            let new_map = map.Add (head, map.[head] + 1)
            break_down rest new_map
        else
            let new_map = map.Add (head, 1)
            break_down rest new_map

let get_checksum (str:string) = 
    break_down (str |> Seq.toList) Map.empty
    |> Seq.sortWith (fun a b -> int a.Key - int b.Key) // Sort alphabetically
    |> Seq.sortWith (fun a b -> b.Value - a.Value)     // Sort by ocurrences
    |> Seq.map (fun a -> a.Key)
    |> Seq.filter (fun a -> a >= 'a' && a <= 'z')
    |> Seq.take 5
    |> Seq.map (fun a -> string a)
    |> String.concat ""

let get_number(str:string) =
    str |> Seq.toList
        |> Seq.filter (fun c -> c >= '0' && c <= '9')
        |> Seq.map (fun c -> string c)
        |> String.concat ""
        |> Int32.Parse

let rotate (c:char) (amt:int) =
    match c with
    | a when a >= 'a' && a <= 'z' ->
        ((int c) - (int 'a') + amt) % 26 + (int 'a') |> char
    | _ -> c

let decrypt (str:string) (amt:int) =
    str |> Seq.toList
        |> Seq.map (fun a -> (rotate a amt) |> string)
        |> String.concat ""

let section_split (str:string) =
    let split = str.Split("[")
    get_checksum split.[0], split.[1].[0..4], get_number str, str

let count list =
    list |> Seq.map section_split
         |> Seq.map (fun (a, b, c, _) -> if a = b then c else 0)
         |> Seq.sum

let get_text list =
    list |> Seq.map section_split
         |> Seq.filter (fun (a, b, _, _) -> a = b)
         |> Seq.map (fun (_, _, c, d) -> decrypt d c)
         |> Seq.find (fun d -> d.StartsWith("northpole-object-storage"))
         |> get_number

let input = IO.File.ReadAllLines("./day04.txt")

input |> count |> printfn "%i"
input |> get_text |> printfn "%i"
