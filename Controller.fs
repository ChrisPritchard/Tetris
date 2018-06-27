module Controller
open Model
open GameCore
open Microsoft.Xna.Framework.Input

let gameTickTime = 150.

let keyMap = 
    function 
    | Keys.Left -> Some Command.Left
    | Keys.Right -> Some Command.Right
    | Keys.Up -> Some Command.Rotate
    | Keys.Down -> Some Command.Drop
    | _ -> None

let mutable pressedAndReleased: Keys list = []

let advanceGame (runState: RunState) gameModel = 
    if not <| List.isEmpty runState.keyboard.keysUp then
        pressedAndReleased <- pressedAndReleased @ runState.keyboard.keysUp
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
            let commands = List.map keyMap (pressedAndReleased @ runState.keyboard.pressed) |> List.choose id
            pressedAndReleased <- []
            processTick commands world |> Some