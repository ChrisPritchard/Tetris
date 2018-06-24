module Controller
open Model
open GameCore
open Microsoft.Xna.Framework.Input

let gameTickTime = 200.

let advanceGame (runState: RunState) gameModel = 
    match gameModel with
    | None -> Some startModel
    | Some m when m.state = GameOver -> Some { startModel with gameTicks = m.gameTicks }
    | Some m ->
        let elapsedTicks = float m.gameTicks * gameTickTime
        if runState.elapsed - elapsedTicks < gameTickTime then gameModel
        else
            let newTicks = m.gameTicks + 1
            let m = { m with gameTicks = newTicks }
            
            let keyMap = 
                function 
                | Keys.Left -> Some Command.Left
                | Keys.Right -> Some Command.Right
                | Keys.Up -> Some Command.Rotate
                | Keys.Down -> Some Command.Drop
                | _ -> None
            let command = 
                List.map keyMap runState.keyboard.pressed |> List.tryPick id
                
            let m = processCommand m command
            let m = 
                if command <> None || newTicks % m.ticksBetweenDrops <> 0 then
                    m
                else
                    drop m

            // process lines

            Some m