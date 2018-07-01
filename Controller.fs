module Controller
open Model
open GameCore
open Microsoft.Xna.Framework.Input

let keyMap = 
    function 
    | Keys.Left -> Some Command.Left
    | Keys.Right -> Some Command.Right
    | Keys.Up -> Some Command.Rotate
    | _ -> None

let eventMap =
    function
    | Moved -> "move"
    | Rotated -> "rotate"
    | Dropped -> "drop"
    | Line -> "line"
    | LevelUp -> "levelUp"
    | Blocked -> "blocked"
    | GameOver -> "gameOver"

let advanceGame (runState: RunState) gameModel = 
    match gameModel with
    | None -> 
        Some startModel
    | Some _ when runState.WasJustPressed Keys.Escape -> 
        None
    | Some m when m.event = Some GameOver -> 
        gameModel
    | Some m ->
        let command = List.map keyMap runState.keyboard.pressed |> List.tryPick id
        let isDropPressed = List.contains Keys.Down runState.keyboard.pressed
        let result = Model.advanceGame runState.elapsed command isDropPressed m

        let sound = result.event |> Option.bind (eventMap >> Some)
        match sound with | None -> () | Some s -> runState.playSound s |> ignore
        
        Some result