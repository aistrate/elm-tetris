# Elm Tetris

[Demo](https://aistrate.github.io/elm-tetris/index.html)

## Building and Deploying

### Development:

```
elm make src/Main.elm --output=main.js
```

To deploy, copy `index.html` and `main.js` to the destination folder.

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

To deploy, copy `index.html` and `main.min.js` to the destination folder. Inside `index.html`, change the script tag to point to `main.min.js` instead of `main.js`.
