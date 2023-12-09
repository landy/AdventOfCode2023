module AdventOfCode2023.Day8

open System

type Direction =
    | Left
    | Right

type Coordinate = { Left: string; Right: string }

let parseInstruction (input: char) =
    match input with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith $"Invalid input for parseInstruction {input}"

let parseDirections (input: string) =
    input.ToCharArray() |> Array.map parseInstruction

let parseCoordinate (input: string) =
    let parts =
        input.Split(
            '=',
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )

    let key = parts[0]
    let destinations = parts[1].Split(',', StringSplitOptions.TrimEntries)
    let left = destinations[0].Substring(1)
    let right = destinations[1].Substring(0, destinations[1].Length - 1)

    key, { Left = left; Right = right }

let parseCoordinates (input: string[]) =
    input |> Array.map parseCoordinate |> Map.ofArray


let rec checkDirection
    (input: Map<string, Coordinate>)
    (directions: Direction[])
    (start: string)
    (target: string)
    (steps: int64)
    =
    let startCoordinate = input[start]
    let directionIndex = steps % (int64 directions.Length) |> int
    let direction = directions[directionIndex]

    let nextCoordinate =
        match direction with
        | Left -> startCoordinate.Left
        | Right -> startCoordinate.Right

    if nextCoordinate.EndsWith target then
        steps + 1L, nextCoordinate
    else
        checkDirection input directions nextCoordinate target (steps + 1L)


let findPath
    (input: Map<string, Coordinate>)
    (strategy: Direction[])
    (start: string)
    (target: string)
    =

    checkDirection input strategy start target 0

let findCycleLengths
    (input: Map<string, Coordinate>)
    (strategy: Direction[])
    (start: string)
    (target: string)
    =
    let startCoordinates =
        input
        |> Map.filter (fun k _ -> k.EndsWith start)
        |> Map.keys
        |> Array.ofSeq
        |> Array.map (fun x -> x, 0L)

    let cycleLengths =
        startCoordinates
        |> Array.map (fun (x, _) -> findPath input strategy x target)
        |> Array.map fst

    cycleLengths

let solvePart1 (input: string[]) =
    let directions = parseDirections input[0]
    let coordinates = parseCoordinates input[2..]

    findPath coordinates directions "AAA" "ZZZ"

let rec getLeastCommonDivisor a b =
    let remainder = a % b

    if remainder = 0L then
        b
    else
        getLeastCommonDivisor b remainder

let getLeastCommonMultiple a b = abs (a * b) / getLeastCommonDivisor a b


let solvePart2 (input: string[]) =
    let directions = parseDirections input[0]
    let coordinates = parseCoordinates input[2..]

    findCycleLengths coordinates directions "A" "Z"
    |> Array.reduce getLeastCommonMultiple