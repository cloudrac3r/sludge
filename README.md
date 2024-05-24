# Sludge

This is an interactive fiction made for [Spring Lisp Game Jam 2024](https://itch.io/jam/spring-lisp-game-jam-2024)

## Set up

1. Install Racket. I used Racket 8.12 CS & BC on Linux and Windows.
2. `raco pkg install --auto req-lib` (install a better package installer)
3. `raco req -d` (install dependencies)

## Run

`racket sludge-game/main.rkt`

## Development workflow

### General

Set `debug-mode` to `#t` in `racket sludge-game/main.rkt` and re-run to activate debugging menus.

### Designs

Work in the `sludge-game/designs` folder using any text editor.

Create a new design with lang line `#lang ruckus` and [develop it in the usual Ruckus way](https://docs.racket-lang.org/ruckus/Getting_Your_Ruckus_On.html). Hint: No need to restart the `ruckus-3d` process every time! You can just press F5 to reload the design from the file.

The design cannot be compiled into the game if it has `#lang ruckus`. When you're happy with the design, add it to the game by changing the lang line to `#lang reader "reader.rkt"` and adding the design's file name to the last form in `all.rkt`. However, being part of the game removes F5 reload and increases overall compile time, so `#lang ruckus` is recommended for rapid iteration.

### Story

Work in the `sludge-game/room.rkt` file using DrRacket or Emacs racket-mode.

Write rooms and cutscenes. Press F5 to just run `room.rkt` without the rest of the game. This will quickly check that the syntax and references are all OK. After `room.rkt` runs successfully with no output, run `sludge-game/main.rkt` as usual to experience your new story in the game environment.

## License, acknowledgements, folder structure

This repository is cloned from [Ruckus](https://github.com/cbiffle/ruckus). Ruckus is a language for describing solid 2D and 3D objects, and includes a renderer. I have modified this repo to add the rest of the game, including the models, text, structures, and logic associated with the game. I have also modified Ruckus to integrate it with the rest of the game and to fix some bugs.

Everything for Sludge is located in the `sludge-game` folder; the other folders are from Ruckus.

See `LICENSE.txt` and `sludge-game/LICENSE.txt`.
