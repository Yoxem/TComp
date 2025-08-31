# TComp
A practice of Essential of Complication in Julia

## Dependencies
 - julia
 - [Match.jl](github.com/JuliaServices/Match.jl)

## instruction
`./src/TComp.jl [.tc file]`

the output assembly code is `./a.c` in AT&T assembly langauge.

to make it executable, please use `gcc`: `gcc ./a.c -o output.out`

the example `.tc` file is in `./test`

## Known issues
 - parser for a + b + c .. and a * b * c