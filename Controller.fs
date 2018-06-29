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

let advanceGame (runState: RunState) gameModel = 
    let newCommands = List.map keyMap runState.keyboard.pressed |> List.choose id
    match gameModel with
    | None -> 
        Some startModel
    | Some _ when runState.WasJustPressed Keys.Escape -> 
        None
    | Some m when m.state = GameOver -> 
        Some { startModel with gameTicks = m.gameTicks }
    | Some m ->
        let elapsedTicks = float m.gameTicks * gameTickTime
        if runState.elapsed - elapsedTicks < gameTickTime then 
            Some { m with commandBuffer = m.commandBuffer @ newCommands }
        else
            let commands = m.commandBuffer |> List.distinct
            let world = { m with gameTicks = m.gameTicks + 1; commandBuffer = [] }
            processTick commands world |> Some