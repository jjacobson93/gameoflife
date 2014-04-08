Game of Life in OCaml
==========

[Conway's Game of Life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), a cellular automaton, simulated in OCaml.

The "game" follows these four simple rules:


1. Any live cell with fewer than two live neighbours dies, as if caused by under-population.
2. Any live cell with two or three live neighbours lives on to the next generation.
3. Any live cell with more than three live neighbours dies, as if by overcrowding.
4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

## Dependencies

Standard OCaml libraries

## Getting Started

1. Run `make` in the root directory: 

```
make life
```

2. Now you can run `life`!

```
# Default options
./life

# Help
./life -h

# Patterns + Size + Delay
./life glidergun 80 40 0.5 # 0.5 second delay
./life spaceship 60 30 1 # 1 second delay
./life pulsar 50 50 0.1 # 0.1 second delay
```
