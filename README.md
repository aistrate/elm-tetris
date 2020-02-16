# Elm Tetris

Elm Tetris is a game of [Tetris](https://en.wikipedia.org/wiki/Tetris) implemented in the [Elm](<https://en.wikipedia.org/wiki/Elm_(programming_language)>) programming language.

See a live [Demo](https://aistrate.github.io/demo/elm-tetris/index.html) here.

## Building and Deploying

The project uses Elm version **0.19**.

See instructions for installing the latest Elm [here](https://guide.elm-lang.org/install/elm.html). Historical versions can be installed from [here](https://github.com/elm/compiler/releases).

### Development:

```
elm make src/Main.elm --output=main.js
```

To deploy, copy `index.html`, `main.js` and `favicon.ico` to the destination folder.

### Production:

```
elm make src/Main.elm --output=main.js --optimize

uglifyjs main.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=main.min.js
```

As an example, this would decrease .js file size from 171 kB to 33 kB.

To install `uglifyjs`:

```
npm install uglify-js --global
```

See [here](https://github.com/elm/compiler/blob/master/hints/optimize.md) for details on how to optimize asset size in Elm.

To deploy, copy `index.html`, `main.min.js` and `favicon.ico` to the destination folder. Inside `index.html`, change the script tag to point to `main.min.js` instead of `main.js`.
