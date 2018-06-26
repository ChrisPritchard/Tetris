module Model

let width, height = 10, 20
let startPos = (width / 2 - 1, 0)
let scorePerLine = 100
let scorePerLevel = 1000
let ticksForLinePause = 20
let random = new System.Random ()

type World = {
    state: State
    score: int
    level: int
    gameTicks: int
    ticksBetweenDrops: int
    currentPause: int
    staticBlocks: (Colour * int * int) list
    pos: int * int
    shape: Colour * ShapeBlock list list
    nextShape: Colour * ShapeBlock list list
    event: Event option
} 
and State = | Playing | GameOver
and ShapeBlock = | X | O
and Colour = | Red | Magenta | Yellow | Cyan | Blue | Silver | Green
and Event = | Moved | Rotated | Dropped | Line

type Command = | Left | Right | Rotate | Drop

let shapes = [
    Cyan, [
        [X;X]
        [X;X]
    ]
    Red, [
        [X;X;X;X]
    ]
    Green, [
        [X;X;O]
        [O;X;X]
    ]
    Blue, [
        [O;X;X]
        [X;X;O]
    ]
    Yellow, [
        [X;X;X]
        [X;O;O]
    ]
    Magenta, [
        [X;X;X]
        [O;O;X]
    ]
    Silver, [
        [X;X;X]
        [O;X;O]
    ]
]

let startModel = {
    state = Playing
    score = 0
    level = 1
    gameTicks = 0
    ticksBetweenDrops = 10
    currentPause = 0
    staticBlocks = []
    pos = startPos
    shape = shapes.[random.Next(shapes.Length)]
    nextShape = shapes.[random.Next(shapes.Length)]
    event = None
}

let rec rotate = function
    | (_::_)::_ as m -> 
        (List.map List.head m |> List.rev)::(List.map List.tail m |> rotate) 
    | _ -> []

let plot (tlx, tly) =
    List.mapi (fun y -> 
        List.mapi (fun x -> function
        | X -> (x + tlx, y + tly) |> Some
        | O -> None) >> List.choose id) >> List.concat

let processCommand command world =
    let (x, y) = world.pos
    let (nx, ny) = 
        match command with
        | Some Left -> (x - 1, y)
        | Some Right -> (x + 1, y)
        | Some Drop -> (x, y + 1)
        | _ -> (x, y)
    
    let newShape = if command = Some Rotate then rotate <| snd world.shape else snd world.shape
    let newBlocks = plot (nx, ny) newShape
    let outOfBounds = newBlocks |> List.exists (fun (x,y) -> x < 0 || x >= width || y < 0 || y >= height)
    let worldBlocks = world.staticBlocks |> List.map (fun (_,x,y) -> x,y)

    if outOfBounds || List.except worldBlocks newBlocks <> newBlocks then 
        None
    else
        let event = 
            match command with
            | Some Rotate -> Some Rotated
            | Some Left | Some Right -> Some Moved
            | Some Drop -> Some Dropped
            | _ -> None
        Some { world with shape = fst world.shape, newShape; pos = (nx, ny); event = event }

let drop world = 
    let (x, y) = world.pos
    let newPos = (x, y + 1)

    let newBlocks = plot newPos <| snd world.shape
    let outOfBounds = newBlocks |> List.exists (fun (x,y) -> x < 0 || x >= width || y < 0 || y >= height)
    let worldBlocks = world.staticBlocks |> List.map (fun (_,x,y) -> x,y)

    if not outOfBounds && List.except worldBlocks newBlocks = newBlocks then 
        { world with pos = newPos }
    else    
        let currentBlocks =
            plot world.pos (snd world.shape)
            |> List.map (fun (x,y) -> fst world.shape, x, y)
        let nextBlocks = plot startPos <| snd world.nextShape
        { world with 
            state = if List.except worldBlocks nextBlocks <> nextBlocks then GameOver else Playing
            staticBlocks = world.staticBlocks @ currentBlocks
            pos = startPos
            shape = world.nextShape
            nextShape = shapes.[random.Next(shapes.Length)] }

let removeLines world = 
    let lines = 
        world.staticBlocks 
            |> List.groupBy (fun (_,_,y) -> y) 
            |> List.filter (fun r -> List.length (snd r) = width)
            |> List.map snd
    let newScore = List.length lines * scorePerLine
    let newLevel = float newScore / float scorePerLevel |> floor |> int |> (+) 1
    { world with 
        staticBlocks = List.except (List.concat lines) world.staticBlocks
        currentPause = if List.isEmpty lines then 0 else ticksForLinePause
        score = newScore
        level = newLevel }

type PhaseResult = | Stop of World | Continue of World 

let pausePhase world = 
    if world.currentPause > 0 then 
        Stop { world with currentPause = world.currentPause - 1 } 
    else Continue world

let canDrop world = world.gameTicks % world.ticksBetweenDrops = 0

let commandPhase command world =
    if command <> None || not <| canDrop world then
        match processCommand command world with
        | None when canDrop world -> Continue world
        | None -> Stop world
        | Some w -> Stop w
    else
        Continue world

let dropPhase world = 
    Continue <| drop world

let linePhase world = 
    Continue <| removeLines world

let finish = function | Stop result | Continue result -> result

let (|=>) phase1result phase2action : PhaseResult =
    match phase1result with
    | Stop world -> Stop world
    | Continue world -> phase2action world

let processTick command world = 
    world |>
        pausePhase |=> commandPhase command |=> dropPhase |=> linePhase
        |> finish