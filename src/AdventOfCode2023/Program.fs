open AdventOfCode2023

let day7Input = loadInputFile 7

let foo = Array.map Day7.parseRow day7Input
let bar = foo |> Day7.sortInputs
let day7ResultPart1 = Day7.solvePart1 day7Input
printfn $"Day 7 - Part 1: %i{day7ResultPart1}"

let day7ResultPart2 = Day7.solvePart2 day7Input
printfn $"Day 7 - Part 2: %i{day7ResultPart2}"