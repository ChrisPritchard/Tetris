module Model

let width, height = 10, 20
let startPos = (width / 2 - 1, 0)
let scorePerLine = 100
let scorePerLevel = 1000

let timeBetweenCommands = 200.
let timeBetweenLines = 1000.
let timeBetweenDrops = 1000.
let minDropTime = 100.
let levelAdjustOnDropTime = 100.

let random = new System.Random ()

type World = {
    score: int
    level: int

    lastCommandTime: float
    lastDropTime: float
    lastLineTime: float
    linesToRemove: (Colour * int * int) list option

    staticBlocks: (Colour * int * int) list
    pos: int * int
    shape: (Colour * ShapeBlock list list) option
    nextShape: Colour * ShapeBlock list list
    events: Event list
} 
and ShapeBlock = | X | O
and Colour = | Red | Magenta | Yellow | Cyan | Blue | Silver | Green
and Event = | Moved | Rotated | Blocked | Dropped | Line | LevelUp | GameOver
and Command = | Left | Right | Rotate

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

let randomShape () = shapes.[random.Next(shapes.Length)]

let startModel = {
    score = 0
    level = 0

    lastCommandTime = 0.
    lastDropTime = 0.
    lastLineTime = 0.
    linesToRemove = None
    
    staticBlocks = []
    pos = startPos
    shape = randomShape () |> Some
    nextShape = randomShape ()
    events = []
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

let isOutOfBounds blocks =
    blocks |> List.exists (fun (x,y) -> x < 0 || x >= width || y < 0 || y >= height)

let isOverlapping blocks world =
    let worldBlocks = world.staticBlocks |> List.map (fun (_,x,y) -> x,y)
    List.except worldBlocks blocks <> blocks

let processCommand elapsed command world =
    match command with
    | None -> world
    | Some c ->
        match world.shape with
        | None -> world
        | Some _ when elapsed - world.lastCommandTime < timeBetweenCommands -> world
        | Some (colour, blocks) ->
            let (x, y) = world.pos
            let (nx, ny) = 
                match c with
                | Left -> (x - 1, y)
                | Right -> (x + 1, y)
                | Rotate -> (x, y)
            
            let newShape = blocks |> match c with | Rotate -> rotate | _ -> id
            let newBlocks = plot (nx, ny) newShape
            
            if isOutOfBounds newBlocks || isOverlapping newBlocks world then 
                { world with events = Blocked::world.events }
            else
                let event = 
                    match c with
                    | Rotate -> Rotated
                    | Left | Right -> Moved
                { world with 
                    shape = Some (colour, newShape)
                    pos = (nx, ny)
                    events = event::world.events
                    lastCommandTime = elapsed }

let drop elapsed isDropKeyPressed world = 
    match world.shape with
    | None -> world
    | Some _ when 
        let timeBetweenDrops = 
            if isDropKeyPressed then minDropTime 
            else timeBetweenDrops - (float world.level * levelAdjustOnDropTime) |> max minDropTime
        elapsed - world.lastDropTime < timeBetweenDrops -> world
    | Some (colour, blocks) ->
        let (x, y) = world.pos
        let newPos = (x, y + 1)

        let newBlocks = plot newPos blocks
        if not (isOutOfBounds newBlocks) && not (isOverlapping newBlocks world) then 
            { world with pos = newPos; lastDropTime = elapsed; events = Dropped::world.events }
        else    
            let currentBlocks = plot world.pos blocks |> List.map (fun (x,y) -> colour, x, y)
            { world with staticBlocks = world.staticBlocks @ currentBlocks; shape = None }

let nextShape world =
    match world.shape with
    | Some _ -> world
    | None ->
        let nextBlocks = snd world.nextShape |> plot startPos
        let isGameOver = isOverlapping nextBlocks world
        { world with
            events = if isGameOver then [GameOver] else world.events
            pos = startPos
            shape = Some world.nextShape
            nextShape = randomShape () }
        
let getLines world = 
    world.staticBlocks 
        |> List.groupBy (fun (_,_,y) -> y) 
        |> List.filter (fun r -> List.length (snd r) = width)
        |> List.collect snd 

let removeLines elapsed world = 
    match world.linesToRemove with
    | None -> world
    | Some lines ->
        let newScore = List.length lines / width * scorePerLine |> (+) world.score
        let newLevel = float newScore / float scorePerLevel |> floor |> int
        
        let horizontals = lines |> List.map  (fun (_,_,y) -> y) |> List.distinct
        let newBlocks = 
            List.except lines world.staticBlocks
            |> List.map (fun (c, x, y) -> 
                let adjust = y::horizontals |> List.sortByDescending id |> List.findIndex ((=) y)
                c, x, (y + adjust))

        { world with 
            staticBlocks = newBlocks
            score = newScore
            level = newLevel
            events = if newLevel <> world.level then LevelUp::world.events else world.events
            lastDropTime = elapsed
            lastCommandTime = elapsed
            linesToRemove = None }

let advanceGame elapsed command isDropPressed world =
    if elapsed - world.lastLineTime < timeBetweenLines then 
        { world with events = [] }
    else
        let result =
            { world with events = [] } 
            |> removeLines elapsed
            |> nextShape
            |> processCommand elapsed command
            |> drop elapsed isDropPressed
        let lines = getLines result
        if List.isEmpty lines then result else 
            { result with 
                events = Line::world.events
                lastLineTime = elapsed
                linesToRemove = Some lines }