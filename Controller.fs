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

let advanceGame (runState: RunState) gameModel = 
    match gameModel with
    | None -> 
        Some startModel
    | Some _ when runState.WasJustPressed Keys.Escape -> 
        None
    | Some m when m.state = GameOver -> 
        gameModel
    | Some m ->
        let command = List.map keyMap runState.keyboard.pressed |> List.tryPick id
        let isDropPressed = List.contains Keys.Down runState.keyboard.pressed
        Model.advanceGame runState.elapsed command isDropPressed m |> Some