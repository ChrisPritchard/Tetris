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
    match gameModel with
    | None -> 
        Some startModel
    | Some _ when runState.WasJustPressed Keys.Escape -> 
        None
    | Some m when m.state = GameOver -> 
        Some { startModel with gameTicks = m.gameTicks }
    | Some m ->
        let elapsedTicks = float m.gameTicks * gameTickTime
        if runState.elapsed - elapsedTicks < gameTickTime then gameModel
        else
            let world = { m with gameTicks = m.gameTicks + 1 }
            let command = List.map keyMap runState.keyboard.pressed |> List.tryPick id
            processTick command world |> Some