#load "Helpers.fsx"
#time "on"

open Helpers
open System

let input = loadInputFile 6

let calculateRaceResult holdDuration neededDistance maximumDuration =
    let racingDuration = maximumDuration - holdDuration
    let racingDistance = racingDuration * holdDuration
    racingDistance > neededDistance

let calculateWinningScenarios (neededDistance: int64) (maximumDuration: int64) =
    [| 1L..maximumDuration |]
    |> Array.Parallel.filter (fun holdDuration ->
        calculateRaceResult holdDuration neededDistance maximumDuration)
    |> Array.length


let parseValues (input: string) =
    input.Split(' ')
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.tail
    |> Array.map int

let parseValuesPart2 (input: string) =
    input.Substring(input.IndexOf(':') + 1).Replace(" ", "") |> int64

let parseInput (input: string[]) =
    let times = parseValues input[0]
    let distances = parseValues input[1]
    Array.zip times distances

let parseInputPart2 (input: string[]) =
    let time = parseValuesPart2 input[0]
    let distance = parseValuesPart2 input[1]
    (time, distance)

let calculatePart1Results (input: string[]) =
    let parsedInput = parseInput input

    parsedInput
    |> Array.map (fun (time, distance) ->
        calculateWinningScenarios distance time)
    |> Array.reduce (*)

let calculatePart2Results (input: string[]) =
    let time, distance = parseInputPart2 input
    calculateWinningScenarios distance time

//
// let part1Result = calculatePart1Results input
// printfn "Part 1: %A" part1Result

let part2Result = calculatePart2Results input
printfn "Part 2: %A" part2Result