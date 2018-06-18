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

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []