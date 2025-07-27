open System

let get_checksum (str:string) = 
    str
    |> Seq.countBy id
    |> Seq.sortBy (fun (k, _) -> int k)       // Sort alphabetically
    |> Seq.sortByDescending (fun (_, v) -> v) // Sort by ocurrences
    |> Seq.filter (fun (k,_) -> k >= 'a' && k <= 'z')
    |> Seq.take 5
    |> Seq.map (fun (k,_) -> string k)
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
    String.concat "" [ for c in str -> rotate c amt |> string ]

let section_split (str:string) =
    let split = str.Split("[")
    get_checksum split.[0], split.[1].[0..4], get_number str, str

let count list =
    List.sum [ for (a,b,c,_) in list |> Seq.map section_split -> if a = b then c else 0 ]

let get_text list =
    list |> Seq.map section_split
         |> Seq.filter (fun (a, b, _, _) -> a = b)
         |> Seq.map (fun (_, _, c, d) -> decrypt d c)
         |> Seq.find (fun d -> d.StartsWith("northpole-object-storage"))
         |> get_number

let input = IO.File.ReadAllLines("./day04.txt")

input |> count |> printfn "%i"
input |> get_text |> printfn "%i"
