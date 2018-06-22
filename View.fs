module View

open GameCore
open Model
open Microsoft.Xna.Framework

let resolution = Windowed (400,600)

let assetsToLoad = [
    Texture { key = "blank"; path = "Content/white" }
    Texture { key = "block"; path = "Content/block" }
    Font { key = "default"; path = "Content/coders_crux" }
]

// Game space
let gx, gy, gw, gh = 10, 10, 250, 500
// Next block space
let nx, ny, nw, nh = 270, 10, 120, 70
// Block size
let bw, bh = 25, 25

let colorFor colour = 
    match colour with
    | Red -> Color.Red | Magenta -> Color.Magenta | Yellow -> Color.Yellow 
    | Cyan -> Color.Cyan | Blue -> Color.Blue | Silver -> Color.Silver | Green -> Color.Green

let posFor (x,y) (ox, oy) = 
    x * bw + ox, y * bh + oy, bw, bh

let getView _ (model: World) = 
    let gameSpace = [
        ColouredImage (Color.Black, { assetKey = "blank"; destRect = gx-1, gy-1, gw+2, gh+2; sourceRect = None })
        ColouredImage (Color.Gray, { assetKey = "blank"; destRect = gx, gy, gw, gh; sourceRect = None })
    ]
    
    let nextBlockSpace = [
        ColouredImage (Color.Black, { assetKey = "blank"; destRect = nx-1, ny-1, nw+2, nh+2; sourceRect = None })
        ColouredImage (Color.Gray, { assetKey = "blank"; destRect = nx, ny, nw, nh; sourceRect = None })
    ]

    let staticBlocks = 
        model.staticBlocks
        |> List.map (fun (c,x,y) ->
            ColouredImage (colorFor c, { assetKey = "block"; destRect = posFor (x,y) (gx, gy); sourceRect = None }))

    let colour = colorFor <| fst model.shape
    let currentShape = 
        (plot (model.pos) <| snd model.shape)
            |> List.map (fun (x,y) ->
                ColouredImage (colour, { assetKey = "block"; destRect = posFor (x,y) (gx, gy); sourceRect = None }))

    let nextColour = colorFor <| fst model.nextShape
    let nextShape = 
        (plot (0, 0) <| snd model.nextShape)
            |> List.map (fun (x,y) ->
                ColouredImage (nextColour, { assetKey = "block"; destRect = posFor (x,y) (nx + 10, ny + 10); sourceRect = None }))

    gameSpace @ nextBlockSpace @ staticBlocks @ currentShape @ nextShape