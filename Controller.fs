module Controller
open Model
open GameCore
open Microsoft.Xna.Framework.Input

let gameTickTime = 100.

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
                
            if m.currentPause > 0 then
                Some { m with currentPause = m.currentPause - 1 }
            else if command <> None || newTicks % m.ticksBetweenDrops <> 0 then
                processCommand m command |> Some
            else
                let dropped = drop m
                let lines = fullLines dropped
                if List.isEmpty lines then
                    Some dropped
                else
                    let withoutLines = removeLines lines dropped
                    let newScore = dropped.score + List.length lines * scorePerLine
                    if newScore % scorePerLevel = 0 then
                        { withoutLines with 
                            score = newScore
                            level = m.level + 1
                            currentPause = ticksForLinePause
                            ticksBetweenDrops = max 1 (m.ticksBetweenDrops - 1) } |> Some
                    else
                        { withoutLines with 
                            score = newScore
                            currentPause = ticksForLinePause } |> Some