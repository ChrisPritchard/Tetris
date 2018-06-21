module Model

let width,height = 10,20
let startPos = (width / 2, 0)

type ShapeBlock = | X | O

let shapes = [
    [
        [X;X]
        [X;X]
    ]
    [
        [X;X;X;X]
    ]
    [
        [X;X;O]
        [O;X;X]
    ]
    [
        [O;X;X]
        [X;X;O]
    ]
    [
        [X;X;X]
        [X;O;O]
    ]
    [
        [X;X;X]
        [O;O;X]
    ]
    [
        [X;X;X]
        [O;X;O]
    ]
]

let rec rotate = function
    | (_::_)::_ as m -> 
        (List.map List.head m |> List.rev)::(List.map List.tail m |> rotate) 
    | _ -> []

let plot (tlx, tly) =
    List.mapi (fun y -> 
        List.mapi (fun x -> function
        | X -> (x + tlx, y + tly) |> Some
        | O -> None) >> List.choose id) >> List.concat

type World = {
    score: int
    timeBetweenDrops: float
    staticBlocks: (int * int) list
    pos: int * int
    shape: ShapeBlock list list
    nextShape: ShapeBlock list list
}

type Commands = | None | Left | Right | Rotate | Drop

let processCommand world command =
    let (x, y) = world.pos
    let (nx, ny) = 
        match command with
        | None | Rotate -> (x, y)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)
        | Drop -> (x, y + 1)
        
    let newShape = if command = Rotate then rotate world.shape else world.shape
    let newBlocks = plot (nx, ny) newShape

    if List.except world.staticBlocks newBlocks <> newBlocks then world
    else { world with shape = newShape; pos = (nx, ny) }

let random = new System.Random ()

let drop world = 
    let (x, y) = world.pos
    let newPos = (x, y + 1)

    let newBlocks = plot newPos world.shape
    if List.except world.staticBlocks newBlocks <> newBlocks then
        let currentBlocks = plot world.pos world.shape
        { world with 
            staticBlocks = world.staticBlocks @ currentBlocks
            pos = startPos
            shape = world.nextShape
            nextShape = shapes.[random.Next(shapes.Length)] }
    else { world with pos = newPos }

let fullLines world = 
    world.staticBlocks 
        |> List.groupBy snd 
        |> List.filter (fun r -> List.length (snd r) = width)
        |> List.map snd

let removeLines lines world = 
    { world with staticBlocks = List.except (List.concat lines) world.staticBlocks }