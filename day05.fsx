open System.Text
open System.Security.Cryptography

let get_hash (str:string) =
    use md5 = MD5.Create()
    Encoding.UTF8.GetBytes(str)
    |> md5.ComputeHash
    |> Seq.map (fun b -> System.String.Format("{0:x2}", b))
    |> String.concat ""

let leading_zeros (h:string) = h.StartsWith("00000")

let find_password (key:string) =
    Seq.initInfinite
        (fun x -> get_hash (String.concat "" [key; string x]))
    |> Seq.filter leading_zeros
    |> Seq.take 8
    |> Seq.map (fun x -> string x.[5])
    |> String.concat ""

let has_valid_position (str:string) = str.[5] >= '0' && str.[5] < '8'
let is_password_full str = str |> Seq.forall (fun c -> c <> ' ')

let find_password_loc (key:string) = 
    Seq.initInfinite (fun x -> get_hash (String.concat "" [key; string x]))
    |> Seq.filter leading_zeros
    |> Seq.filter has_valid_position
    |> Seq.map (fun x -> (int (x.[5] - '0'), x.[6]))
    |> Seq.scan
        (fun a (i, c) -> Array.mapi (fun j d -> if i = j && d = ' ' then c else d) a)
        (Array.init 8 (fun _ -> ' '))
    |> Seq.find is_password_full
    |> Seq.map string
    |> String.concat ""

// these are dog slow and i'd like to believe it's because of the hashing
// let's try multithreading this at some point, i guess
find_password "ojvtpuvg" |> printfn "%s"
find_password_loc "ojvtpuvg" |> printfn "%s"
