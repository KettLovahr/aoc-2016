open System
open System.Text
open System.Security.Cryptography

let get_hash (str:string) =
    use md5 = MD5.Create()
    Encoding.UTF8.GetBytes(str)
    |> md5.ComputeHash
    |> Seq.map (fun b -> System.String.Format("{0:X2}", b))
    |> String.concat ""


let rec find_password key num (pass:string) =
    if pass.Length = 8 then
        printfn ""
        pass
    else
        let h = get_hash (String.concat "" [key; string num])
        match h.[0..4] = "00000" with
        | true -> let new_pass = String.concat "" [pass; string h.[5]]
                  printf "%c" h.[5]
                  find_password key (num+1) new_pass
        | false -> find_password key (num+1) pass

let is_valid_position c = c >= '0' && c < '8'

let is_password_full str = str |> Seq.toList |> Seq.forall (fun c -> c <> ' ')

let rec find_password_loc key num (pass:string) =
    if pass |> is_password_full then
        pass
    else
        let h = get_hash (String.concat "" [key; string num])
        match h.[0..4] = "00000" && is_valid_position h.[5] with
        | true -> let pos = h.[5] |> string |> Int32.Parse
                  if pass.[pos] = ' ' then
                      let new_pass = String.concat "" [pass.Substring(0, pos); string h.[6]; pass.Substring(pos + 1)]
                      printfn "%s" new_pass
                      find_password_loc key (num+1) new_pass
                  else
                      find_password_loc key (num+1) pass
        | false -> find_password_loc key (num+1) pass
    

// these are dog slow and i'd like to believe it's because of the hashing
// let's try multithreading this at some point, i guess
find_password "ojvtpuvg" 0 "" |> printfn "%s"
find_password_loc "ojvtpuvg" 0 "        " |> printfn "%s"
