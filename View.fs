module View

open GameCore

let resolution = Windowed (300,600)

let assetsToLoad = [
    Texture { key = "blank"; path = "Content/white" }
    Texture { key = "explosion"; path = "Content/explosion" }
    Texture { key = "cursor"; path = "Content/cursor" }
    Font { key = "default"; path = "Content/miramo" }
]

let getView runState model = 
    []