# Elm Tetris

Elm Tetris is a game of [Tetris](https://en.wikipedia.org/wiki/Tetris) implemented in the [Elm](<https://en.wikipedia.org/wiki/Elm_(programming_language)>) programming language.

See a live [Demo](https://aistrate.github.io/demo/elm-tetris/index.html) here.

## Contents

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

## Possible Future Features

- **Mouse-based dialogs** instead of keyboard-based ones
- **Settings dialog**:
  - ability to assign keyboard shortcuts (Move left, Move right, Move down, Rotate clockwise, Rotate counterclockwise, Drop)
  - Lock delay: Limited Spin (default) / Infinite Spin / Step
  - Random generation: 7-Bag (default) / Simple
  - Ghost piece: On (default) / Off
  - Vertical stripes : On / Off (default)
- **Pause/Play button** (alternative to keyboard shortcut P)
- **Hold piece**: storing a falling Tetromino for later use
- Score points for **T-Spins**
- Score points for **Combos** (optional, many games don't have it)
- **Levels above 15**: For now, the higher the level, the higher the speed of the drop animation. Above level 15, the falling piece drops to the bottom almost instantly, so increasing the speed would not increase difficulty. **Lock delay** (now 500 ms) and **maximum lock delay moves** (now 15) will have to gradually decrease in order to make the game more challenging.
- **Game modes**, for example: Marathon (normal), Challenge (10 min), Sprint (40 lines), Master (instant lock to bottom / zero lock delay)
- **Smoother movement** of the falling piece, with less animation flicker, especially on levels 7-12 (if SVG allows it)
- **Play by mouse**, not just by keyboard (as on [tetris.com](https://tetris.com/))
- **Save Settings** locally between visits (to browser localStorage)

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
