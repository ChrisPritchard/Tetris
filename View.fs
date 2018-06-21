module View

open GameCore

let resolution = Windowed (300,600)

let assetsToLoad = [
    Texture { key = "blank"; path = "Content/white" }
    Texture { key = "block"; path = "Content/block" }
    Font { key = "default"; path = "Content/coders_crux" }
]

let getView runState model = 
    []