module Model

let width,height = 10,20

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
    timeBetweenDrops: float
    staticBlocks: (int * int) list
    pos: int * int
    shape: ShapeBlock list list
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