module Controller
open Model
open GameCore
open Microsoft.Xna.Framework.Input

let gameTickTime = 100.

let keyMap = 
    function 
    | Keys.Left -> Some Command.Left
    | Keys.Right -> Some Command.Right
    | Keys.Up -> Some Command.Rotate
    | Keys.Down -> Some Command.Drop
    | _ -> None

let getCommand runState = 
    List.map keyMap runState.keyboard.pressed |> List.tryPick id

type PhaseResult = | Stop of World | Continue of World 

let pausePhase world = 
    if world.currentPause > 0 then 
        Stop { world with currentPause = world.currentPause - 1 } 
    else Continue world

let commandPhase runState world =
    let command = getCommand runState
    if command <> None || world.gameTicks % world.ticksBetweenDrops <> 0 then
        Stop <| processCommand world command
    else
        Continue world

let dropPhase world = 
    Continue <| drop world

let linePhase world = 
    Continue <| removeLines world

let (|=>) phase1 phase2action : PhaseResult =
    match phase1 with
    | Stop world -> Stop world
    | Continue world -> phase2action world

let advanceGame (runState: RunState) gameModel = 
    match gameModel with
    | None -> Some startModel
    | Some _ when runState.WasJustPressed Keys.Escape -> None
    | Some m when m.state = GameOver -> Some { startModel with gameTicks = m.gameTicks }
    | Some m ->
        let elapsedTicks = float m.gameTicks * gameTickTime
        if runState.elapsed - elapsedTicks < gameTickTime then gameModel
        else
            let newTicks = m.gameTicks + 1
            let world = { m with gameTicks = newTicks }
            world |>
                pausePhase |=> commandPhase runState |=> dropPhase |=> linePhase
                |> function | Stop result | Continue result -> Some result