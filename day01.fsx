open System

type Direction = | North | East | South | West

let left_of = function
              | North -> West | West -> South
              | South -> East | East -> North

let right_of = function
               | North -> East | West -> North
               | South -> West | East -> South

let move_by dir dist = match dir with
                       | North -> 0, -dist
                       | South -> 0, dist
                       | West  -> -dist, 0
                       | East  -> dist, 0

let addv2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let move pos dir (inst:string) locations =
     let (turn, steps) = inst.[0], (inst.[1..] |> Int32.Parse)
     let new_dir = match turn with
                   | 'L' -> left_of dir
                   | 'R' -> right_of dir
                   | _ -> failwith "unexpected direction"
     let new_locations = [1..steps] |> List.map (fun x -> addv2 pos (move_by new_dir x))
     (List.last new_locations), new_dir, (List.append locations new_locations)

let rec find_first_repeat list history =
    match list with
    | [] -> failwith "didn't find a repeat"
    | first::rest -> if List.contains first history then first
                     else find_first_repeat rest (List.append history [first])

let input = IO.File.ReadAllText("./day01.txt").Split(", ")

let ((x, y), _, locations) = input
                             |> Seq.fold
                                (fun ((x, y), dir, locations) inst -> move (x, y) dir inst locations)
                                ((0, 0), North, [])

// Part 1
Math.Abs(x) + Math.Abs(y) |> printfn "%i"
// Part 2
find_first_repeat locations [] |> (fun (x, y) -> printfn "%i" (Math.Abs(x) + Math.Abs(y)))
