open System

let is_abba (str:char array) =
    match str.Length = 4 with
    | false -> false
    | true -> str.[0] = str.[3] && str.[1] = str.[2] && str.[0] <> str.[1]

let has_abba (str:string) =
    str |> Seq.windowed 4 |> Seq.exists is_abba

let is_aba (str:char array) =
    match str.Length = 3 with
    | false -> None
    | true -> if str.[0] = str.[2] && str.[0] <>  str.[1] then
                  Some(str.[0], str.[1])
              else
                  None

let get_abas (str:string) =
    str |> Seq.windowed 3
        |> Seq.map is_aba
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.map (fun x -> x.Value)

let has_bab (str:string) (a:char) (b:char) =
    str |> get_abas
        |> Seq.exists (fun x -> x = (b, a))

let get_hypernet (str:string) =
    let list = str.Split("[")
               |> Seq.filter (fun s -> s.Contains("]")) 
               |> Seq.map (fun s -> s.Split("]") |> Seq.head) 
    list

let get_supernet (str:string) =
    let list = str.Split("[")
               |> Seq.map (fun s -> s.Split("]") |> Seq.last) 
    list

let supports_tls (str:string) =
    (has_abba str) && not (str |> get_hypernet |> Seq.exists has_abba)

let supports_ssl (str:string) = 
    let abas_available =
        str |> get_supernet
            |> Seq.map get_abas
            |> Seq.concat
    abas_available
    |> Seq.map (fun (a, b) -> get_hypernet str
                              |> Seq.map (fun x -> has_bab x a b)
                              |> Seq.exists (fun x -> x))
    |> Seq.exists (fun x -> x)

let solve list f =
    Seq.sum [ for i in list -> if f i then 1 else 0 ]

let input = IO.File.ReadAllLines("./day07.txt")

solve input supports_tls |> printfn "%i"
solve input supports_ssl |> printfn "%i"
