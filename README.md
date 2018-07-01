# Tetris

Classic tetris, implemented in F# with MonoGame. An exercise in functional programming and game solution design.

<img align="center" alt="screenshot" src="./screenshot.png" />

## Supported platforms

Being dotnet core 2.1, it should work on all platforms that supports (Windows, Linux, Mac). Tested (and largely coded on) Windows 10. A full list of dotnet core supported platforms can be found from here: <https://github.com/dotnet/core/blob/master/release-notes/2.1/2.1-supported-os.md>

I built this using VS Code, but have also tested opening and running on Visual Studio 2017.

A note for mac users: part of the compilation of this game involves building the content, done using a MonoGame content builder referenced via Nuget. On OSX, this component does not work with just dotnet core. I have managed to get it going by doing the following:

- Installing the latest version of LTS Mono from here (version 5.12.0): <http://www.mono-project.com/download/stable/#download-mac>
- Installing the latest version of the MonoGame standalone pipeline builder for OSX from here (Pipeline.MacOS.pkg, v3.6): <http://www.monogame.net/2017/03/01/monogame-3-6/>
- Doing a sudo dotnet restore and a sudo dotnet build

After the build succeeded, a sudo dotnet run started the game without issue.

## Acknowledgements

The game and code is Unilicense, but I have used two sets of external resources:

- The font is called "Coders Crux" and is from here: <https://www.dafont.com/coders-crux.font>
- The sounds were acquired from here: <https://opengameart.org/art-search-advanced?keys=&field_art_type_tid%5B%5D=13&sort_by=count&sort_order=DESC>

Unfortunately I got this sound set a long time ago (five plus years) and can't find the exact provenance.

## Guide to components

The five code files in this project are described below, in decreasing game importance. Aside from code, there is also the Content folder which contains images, sounds, fonts etc and the Content.mgcb file, which MonoGame uses to compile assets into the game exe. This compilation is triggered by a line in the Tetris.fsproj file (MonoGameContentReference Include="**\*.mgcb") and done by the builder referenced as a nuget package (MonoGame.Content.Builder). Should be automatic on build (though sometimes you need to build twice).

### Model.fs

This file contains a pure F# representation of Tetris: types and DUs to represent the current game state, and functions to transition from one state to the next. It knows nothing of MonoGame, of inputs, resolutions or views - even Colour is an enum defined in Model.fs (which is later translated by View.fs)

The top of the file contains constants for the game, like score amounts, the game width, shape templates etc. The bulk of the file contains methods for transitioning different parts of the state, like processCommand and drop, while the final method AdvanceGame is the primary point of entry. This final method takes the previous/current state, an optional command (translated from Keys by Controller.fs) and an elapsed game time, and returns a new state.

### Controller.fs

The controller's job is to control when the Model should be advanced, and to feed inputs into it. Primarily, if the game isnt over it will map user keys to Model commands, and pass through the current game time to the controller. Ultimately, the tiny Controller.fs file could be buried in Model.fs if it wasnt for a desire to keep Model.fs purity and hide the MonoGame keys construct. By splitting these out, how the game is controlled becomes an abstraction: blocks could be rotated or moved with the mouse if necessary, by modifying Controller.fs alone.

### View.fs

If Controller.fs was about inputs, then View.fs is about outputs. It takes a model, and translates it to rendered sprites on the screen and sounds to be played. Despite this job, it is still largely agnostic of XNA, translating the model to abstractions which the GameLoop then uses. It is aware of the MonoGame colour construct, however, as mentioned.

### GameCore.fs

GameCore is where all the MonoGame stuff is kept, and is basically the MonoGame.Game game loop class plus a bunch of abstraction types used to keep the other files pure. MonoGame is a class-based, imperative game development framework, that needs mutable types aplenty to work, and in order to keep my game largely free of these impurities, its all buried in here. GameLoop, the key class, is given the entry methods and model type from the other files, and orchestrates them. It could be viewed as the Imperative shell of the game.

### Program.fs

This instantiates the GameLoop, passing through the methods and types from the other files, and handles disposal