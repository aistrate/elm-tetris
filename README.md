# Elm Tetris

Elm Tetris is a game of [Tetris](https://en.wikipedia.org/wiki/Tetris) implemented in the [Elm](<https://en.wikipedia.org/wiki/Elm_(programming_language)>) programming language.

See a live [demo](https://aistrate.github.io/demo/elm-tetris/index.html) here.

## Contents

- [Features](#features)
  - [Lock Delay](#lock-delay)
- [Possible Future Features](#possible-future-features)
- [Developing](#developing)
  - [Installing Elm](#installing-elm)
  - [Reactor](#reactor)
  - [Time-Travelling Debugger](#time-travelling-debugger)
  - [REPL](#repl)
  - [Elm Documentation](#elm-documentation)
- [Building and Deploying](#building-and-deploying)
  - [Development](#development)
  - [Production](#production)

## Features

### Lock Delay

[Lock delay](https://tetris.wiki/Lock_delay) is the time interval between the falling piece reaching the bottom and its locking to the bottom. It's normally 500 ms, but can be extended by player moves or rotations. The goal is to allow the player a few correction moves after reaching the bottom, but not so many that it makes the game unchallenging (and certainly not [infinite spin](https://tetris.wiki/Infinity), floating the piece indefinitely). The current implementation might be called _limited move reset lock delay_. The gory details:

- every time the _deepest row reached_ changes, lock delay is reset to 500 ms, and the _allowed moves counter_ is reset to 15
- when the player makes a move but the _deepest row reached_ does not change, the _allowed moves counter_ is decreased by 1, and lock delay is reset to 500 ms (only actual moves count; trying to move into a wall does not)
- when the player makes a move but the _allowed moves counter_ is 0, lock delay is not extended anymore
- after the end of lock delay (when the 500 ms interval ends), the piece will lock as soon as it touches the bottom
- if the _deepest row reached_ changes again at any point, lock delay is again reset to 500 ms and the _allowed moves counter_ is reset to 15

The values of 500 ms for lock delay and 15 for the reset limit are standard for existing Tetris implementations. See subsection _Lock Down_ of the [Tetris Guideline](https://tetris.wiki/Tetris_Guideline#Indispensable_rules) for details, and for a description of the 3 types of lock delay.

## Possible Future Features

- **Mouse-based dialogs** instead of keyboard-based ones
- **Settings dialog**:
  - reassigning keyboard shortcuts (Move left, Move right, Move down, Rotate clockwise, Rotate counterclockwise, Drop)
  - Lock Delay: Limited Spin (default) / Infinite Spin / Step Reset
  - Random Generation: 7-Bag (default) / Simple
  - Ghost Piece: On (default) / Off
  - Vertical Stripes : On / Off (default)
- **Pause/Play button**, mouse-based alternative to keyboard shortcut P
- [**Hold piece**](https://tetris.wiki/Hold_piece), storing a falling piece for later use
- Award [score points](https://tetris.wiki/Scoring#Recent_guideline_compatible_games) for:
  - [**T-Spin**](https://tetris.wiki/T-Spin), twisting a T-shaped piece into a tight space
  - [**Combos**](https://tetris.wiki/Combo), multiple line clears in quick succession (optional, many games don't have it)
- **Levels above 15**: For now, the higher the level, the higher the speed of the drop animation. Above level 15, the falling piece drops to the bottom almost instantly, so increasing the speed would not increase difficulty. _Lock delay_ (now 500 ms) and _maximum lock delay moves_ (now 15) will have to gradually decrease in order to make the game more challenging.
- **Game modes**, for example (in [Tetris Zone](https://web.archive.org/web/20140701182459/http://zone.tetris.com/page/manual)):
  - Marathon (normal): level will increase for every 10 lines cleared
  - Challenge: highest score in 10 minutes
  - Sprint: fastest time to clear 40 lines
  - Master: same as Marathon, but with instant lock
- **Smoother movement** of the falling piece, with less animation flicker, especially on levels 7-12 (if SVG allows it)
- **Play by mouse**, not just by keyboard (as on [tetris.com](https://tetris.com/))
- **Save settings** locally between visits (into browser localStorage)
- **Play with [pentominos](https://en.wikipedia.org/wiki/Pentomino)** instead of [tetrominos](https://en.wikipedia.org/wiki/Tetromino) (option in Settings dialog)

## Developing

### Installing Elm

Elm Tetris was developed with Elm version **0.19.1**.

Instructions for installing the latest Elm can be found [here](https://guide.elm-lang.org/install/elm.html). Historical versions can be installed [here](https://github.com/elm/compiler/releases).

It's also useful to install the [Elm extension](https://marketplace.visualstudio.com/items?itemName=Elmtooling.elm-ls-vscode) for Visual Studio Code. It has syntax highlighting, points out syntax errors, and formats code on Save.

### Reactor

For day-to-day development, start the `reactor` server:

```
elm reactor
```

Then go to `http://localhost:8000` to see the project dashboard (file viewer). Inside it, navigate to `src/Main.elm`. This will show the full working version of the application (minus the styling in `index.html`).

Every time you edit `Main.elm` or one of its dependencies, just refresh the browser and `reactor` will recompile everything.

### Time-Travelling Debugger

Occasionally it can be useful to rewind and replay events in order to fix tricky bugs. To start the time-travelling debugger, first run:

```
elm make src/Main.elm --output=main.js --debug
```

Then load `index.html` in the browser, which will show the running application with a little Elm icon in the bottom right corner of the page. Click it to open the time-travelling debugger.

Unfortunatelly, this application is subscribing to `Browser.Events.onAnimationFrameDelta`, which will flood the message queue with one message every 16 ms (60 times/s), thus making the use of the time-travelling debugger a bit difficult.

### REPL

Another useful tool is the Elm REPL (read–eval–print loop):

```
elm repl
```

To see a list of all Elm commands (install, etc.), use:

```
elm --help
```

### Elm Documentation

The most useful links:

[Core Libraries](https://package.elm-lang.org/packages/elm/core/latest/)

[Packages](https://package.elm-lang.org/)

[Elm Guide](https://guide.elm-lang.org/)

[Examples](https://elm-lang.org/examples)

## Building and Deploying

### Development

For a quick way to run the application through `index.html` instead of `reactor`:

```
elm make src/Main.elm --output=main.js
```

If needing to deploy, copy `index.html`, `main.js` and `favicon.ico` to the destination folder.

### Production

For a fully optimized/minified version of the application:

```
elm make src/Main.elm --output=main.js --optimize

uglifyjs main.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=main.min.js
```

As an example, this would decrease .js file size from 171 kB to 33 kB.

To deploy, copy `index.html`, `main.min.js` and `favicon.ico` to the destination folder. Inside `index.html`, change the `<script>` tag to point to `main.min.js` instead of `main.js`.

To install `uglifyjs`:

```
npm install uglify-js --global
```

See [here](https://github.com/elm/compiler/blob/master/hints/optimize.md) for details on how to optimize asset size in Elm.
