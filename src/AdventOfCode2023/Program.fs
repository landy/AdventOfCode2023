open AdventOfCode2023

let day = 10
let input = loadInputFile day

let timer = System.Diagnostics.Stopwatch.StartNew()
let part1 = Day10.solvePart1 input
printfn $"Day {day} - Part 1: %i{part1}"

// let part2 = Day10.solvePart2 input
// printfn $"Day {day} - Part 2: %i{part2}"
timer.Stop()

printfn $"Day {day} took {timer.ElapsedTicks |> System.TimeSpan.FromTicks}"
//
// printfn
//     $"Day {day} - Part 2: %i{part2} took {timer.ElapsedTicks |> System.TimeSpan.FromTicks}"