# MechAnts
Simulator used by the Sinfonie association to hold the Sinfourmis tournament.

## General parameters
`--turns <turns>`, `-t <turns>`: Sets the number of turns that are played before a match ends.

`--seed <seed>`, `-s <seed>`: Sets the seed used for PRNG. The seed used for a match is shown when `--verbose` is passed.

`--verbose`, `-v`: Prints out more information.

## Commands
### Match
`mechants match <map> <programs> [--display | -d]`

Plays a match between a set of programs on a given map.

The `--display` (or `-d`) flag allows you to show the state of the map while the match
is played.

### Round-Robin
`mechants round-robin <map> <programs>`

Plays a tournament where every program plays against every other program on a given map.

### Check
`mechants check <program>`

Checks the given `.brain` program for syntax errors, and outputs clear indications
if any error is found.

### ToBin
`mechants to-bin <program> [output]`

Compiles a `.brain` program (text format) to a binary format. This shrinks the size
of the program by a factor of 2 on average, especially efficient for programs with lots
of labels.



