module Model

let width,height = 10,20

type ShapeBlock = | X | O
let shapes = [
    [
        [X;X]
        [X;X]
    ]
    [
        [X;X;X;X]
    ]
    [
        [X;X;O]
        [O;X;X]
    ]
    [
        [O;X;X]
        [X;X;O]
    ]
    [
        [X;X;X]
        [X;O;O]
    ]
    [
        [X;X;X]
        [O;O;X]
    ]
]

let rec rotate = function
    | (_::_)::_ as m -> 
        (List.map List.head m |> List.rev)::(List.map List.tail m |> rotate) 
    | _ -> []

let plot (tlx, tly) =
    List.mapi (fun y -> 
        List.mapi (fun x -> function
        | X -> (x + tlx, y + tly) |> Some
        | O -> None) >> List.choose id)