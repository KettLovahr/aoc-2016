let input = System.IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}/input").Split(", ")

type Direction =
| NORTH = 0 | EAST = 1
| SOUTH = 2 | WEST = 3
| RIGHT = 1 | LEFT = -1

type Coords =
    struct
        val x: int val y: int
        new(x, y) = {x = x; y = y}
    end

type Position =
    struct 
        val coords: Coords val dir: Direction
        new(coords, dir) = {coords = coords; dir = dir}
    end

let move (c_pos: Position, inst: string) =
         let turn = inst[0]
         let steps = System.Int32.Parse(inst.Substring(1))
         let dire = match turn with
                    | 'L' -> if c_pos.dir = Direction.NORTH
                             then Direction.WEST
                             else c_pos.dir + Direction.LEFT
                    | 'R' -> if c_pos.dir = Direction.WEST
                             then Direction.NORTH
                             else c_pos.dir + Direction.RIGHT
                    | _ -> Direction.NORTH //Ideally unreachable
         let offset = match dire with
                      | Direction.NORTH -> new Coords(0, -steps)
                      | Direction.SOUTH -> new Coords(0, steps)
                      | Direction.WEST -> new Coords(-steps, 0)
                      | Direction.EAST -> new Coords(steps, 0)
                      | _ -> new Coords(0, 0) //Ideally unreachable
         let final_x = c_pos.coords.x + offset.x
         let final_y = c_pos.coords.y + offset.y
         new Position (new Coords(final_x, final_y), dire)

let res = input
          |> Array.fold
             (fun a e -> move (a, e))
             (new Position(new Coords(0, 0), Direction.NORTH))

let final_distance = System.Math.Abs(res.coords.x) + System.Math.Abs(res.coords.y)
printfn $"{final_distance}"
