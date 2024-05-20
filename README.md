# Sludge

This is an interactive fiction made for [Spring Lisp Game Jam 2024](https://itch.io/jam/spring-lisp-game-jam-2024)

## Set up

1. Install Racket. I used Racket 8.12 CS & BC on Linux and Windows.
2. `raco pkg install --auto req-lib` (install a better package installer)
3. `raco req -d` (install dependencies)

## Run

`racket sludge-game/main.rkt`

## Debug

Set `debug-mode` to `#t` in `racket sludge-game/main.rkt` and re-run to activate debugging menus.

## License, acknowledgements, folder structure

This repository is cloned from [Ruckus](https://github.com/cbiffle/ruckus). Ruckus is a language for describing solid 2D and 3D objects, and includes a renderer. I have modified this repo to add the rest of the game, including the models, text, structures, and logic associated with the game. I have also modified Ruckus to integrate it with the rest of the game and to fix some bugs.

Everything for Sludge is located in the `sludge-game` folder; the other folders are from Ruckus.

See `LICENSE.txt` and `sludge-game/LICENSE.txt`.
