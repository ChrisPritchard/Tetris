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

let initModel elapsed = 
    Some { startModel with lastDropTime = elapsed; lastCommandTime = elapsed }

let advanceGame (runState: RunState) gameModel = 
    match gameModel with
    | None -> 
        initModel runState.elapsed
    | Some _ when runState.WasJustPressed Keys.Escape -> 
        None
    | Some m when m.isGameOver && runState.WasJustPressed Keys.R -> 
        initModel runState.elapsed
    | Some m when m.isGameOver ->
        Some { m with events = [] }
    | Some m ->
        let command = List.map keyMap runState.keyboard.pressed |> List.tryPick id
        let isDropPressed = List.contains Keys.Down runState.keyboard.pressed
        Model.advanceGame runState.elapsed command isDropPressed m |> Some