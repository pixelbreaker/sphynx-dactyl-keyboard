# Sphynx Lo-pro dacman keyboard

This is a fork of the [Dactyl-ManuForm](https://github.com/tshort/dactyl-keyboard). The Dactyl-Manuform is a fork of the [Dactyl](https://github.com/adereth/dactyl-keyboard) with the thumb cluster from [ManuForm](https://github.com/jeffgran/ManuForm).

Photos: https://imgur.com/gallery/YePWDY5

## Forks

- https://github.com/okke-formsma/dactyl-manuform-tight/
- https://github.com/lebastaq/dactyl-manuform-mini-keyboard

## Features

- As low stack height as possible
- Choc switch support, tight spacing
- Optional encoder and Cirque trackpad options

## Generate OpenSCAD and STL models

- Run `lein repl`
- In the repl run `(load-file "src/sphynx.clj")`
- This will regenerate the `things/*.scad` files
- Use OpenSCAD to open a `.scad` file.
- Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
- When done, use OpenSCAD to export STL files

## Tips

- When trying things out, 10 seconds of rendering time in OpenSCAD is really annoying. Load one of the test outputs with commented out parts that you don't use.
- If you're not sure what things are generted by a piece of code, color them in using something like
  `(color [0.5 0.5 0.5 0.5] (the code)`

## License

Copyright © 2015-2023 Matthew Adereth, Tom Short, Leo Lou, Okke Formsma, Gabes Mak

The source code for generating the models is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).

The generated models are distributed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](LICENSE-models).
