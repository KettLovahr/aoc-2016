My attempt to solve [Advent of Code 2016](https://adventofcode.com/2016) using [F#](https://fsharp.org/) in order to better learn the language. I don't have much experience with functional programming languages, so this is my attempt to get a better feel for the paradigm.

F# does allow for mutability and loops, but it's not "idiomatic," so I opt against using these features, relying mostly on recursion and pattern matching.

The code in this repository targets Mono, not the modern .NET version of F#, but it should be compatible. The most glaring differences I've come across are:
- String interpolation with the `$"{var}"` syntax does not work
- Indexing can only be done with the `var.[x]` syntax, the modern .NET F# allows for indexing with `var[x]` instead
- The `List.transpose` function does not exist. But you can easily [make your own](day06.fsx)
