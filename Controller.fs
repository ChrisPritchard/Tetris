module Controller
open Model
open GameCore

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
            
            // process actions
            
            let m = 
                if newTicks % m.ticksBetweenDrops <> 0 then
                    m
                else
                    drop m

            // process lines

            Some m