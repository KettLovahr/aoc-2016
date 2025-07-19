open System

type Direction = | North | East | South | West

let left_of = function
              | North -> West | West -> South
              | South -> East | East -> North

let right_of = function
               | North -> East | West -> North
               | South -> West | East -> South

let move_by dir dist = match dir with
                       | Direction.North -> 0, -dist
                       | Direction.South -> 0, dist
                       | Direction.West  -> -dist, 0
                       | Direction.East  -> dist, 0

let addv2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let move pos dir (inst:string) =
     let (turn, steps) = inst.[0], (inst.[1..] |> Int32.Parse)
     let new_dir = match turn with
                   | 'L' -> left_of dir
                   | 'R' -> right_of dir
                   | _ -> failwith "unexpected direction"
     let offset = move_by new_dir steps
     let final = addv2 pos offset
     final, new_dir

let ((x, y), _) = IO.File.ReadAllText("./day01.txt").Split(", ")
                  |> Seq.fold
                     (fun ((x, y), dir) inst -> move (x, y) dir inst)
                     ((0, 0), Direction.North)

let final_distance = Math.Abs(x) + Math.Abs(y)
printfn "%i" final_distance
