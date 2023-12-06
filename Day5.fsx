#load "Helpers.fsx"
#time "on"

open System
open Helpers
let testInput =
    [| "seeds: 79 14 55 13"
       ""
       "seed-to-soil map:"
       "50 98 2"
       "52 50 48"
       ""
       "soil-to-fertilizer map:"
       "0 15 37"
       "37 52 2"
       "39 0 15"
       ""
       "fertilizer-to-water map:"
       "49 53 8"
       "0 11 42"
       "42 0 7"
       "57 7 4"
       ""
       "water-to-light map:"
       "88 18 7"
       "18 25 70"
       ""
       "light-to-temperature map:"
       "45 77 23"
       "81 45 19"
       "68 64 13"
       ""
       "temperature-to-humidity map:"
       "0 69 1"
       "1 0 69"
       ""
       "humidity-to-location map:"
       "60 56 37"
       "56 93 4" |]
let input = loadInputFile 5

type Mapping =
    { SourceRangeStart: int64
      DestinationRangeStart: int64
      RangeLength: int64 }

let tryGetDestinationMapping (mapping: Mapping) (source: int64) =
    let startPosition = mapping.SourceRangeStart
    let endPosition = mapping.SourceRangeStart + mapping.RangeLength - (int64 1)

    if source >= startPosition && source <= endPosition then
        Some(mapping.DestinationRangeStart + (source - startPosition))
    else
        None

let findDestination (mapping: Mapping[]) (source: int64) =
    mapping
    |> Array.map (fun m -> tryGetDestinationMapping m source)
    |> Array.tryFind Option.isSome
    |> Option.flatten
    |> Option.defaultValue source

let calculatePath (mapping: Mapping[][]) (source: int64) =
    let r =
        let foldingFn currentSource mapping =
            let destination = findDestination mapping currentSource
            destination
        mapping
        |> Array.fold foldingFn source
    r
let findLowestDestination (mapping: Mapping[][]) (source: int64[]) =
    source |> Array.Parallel.map (calculatePath mapping ) |> Array.min

let parseSeeds (input: string) =
    input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.tail
    |> Array.map int64

let parseSeedsPart2 (input: string) =
    input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.tail
    |> Array.map int64
    |> Array.chunkBySize 2
    |> Array.map (fun [| a; b |] -> [| a ..  a + b - (int64 1) |])
    |> Array.concat
let parseMappingRow (line: string) =
    line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64
    |> (fun [| destinationStart; sourceStart; rangeLength |] ->
        { SourceRangeStart = sourceStart
          DestinationRangeStart = destinationStart
          RangeLength = rangeLength })

let parseMapping (lines: string[]) =
    let withoutHeader = lines |> Array.tail
    withoutHeader |> Array.map parseMappingRow

let parseMappings (input: string[]) =
    let mappingsInInput = input[2..]
    let indexes = mappingsInInput |> Array.indexed

    let dividers =
        indexes
        |> Array.filter (snd >> String.IsNullOrWhiteSpace)
        |> Array.map fst

    let allDividers =
        [| [| 0 |]; dividers; [| Array.length mappingsInInput - 1 |] |]
        |> Array.concat
        |> Array.pairwise


    allDividers
    |> Array.map (fun (start, end_) ->
        let mappingLines =
            mappingsInInput[start..end_]
            |> Array.filter (String.IsNullOrWhiteSpace >> not)

        parseMapping mappingLines)

let parseInput (input: string[]) =
    let seeds = parseSeeds input[0]
    let mappings = parseMappings (input: string[])

    seeds, mappings

let parseInputPart2 (input: string[]) =
    let seeds = parseSeedsPart2 input[0]
    let mappings = parseMappings (input: string[])

    seeds, mappings

let solvePart1 (input: string[]) =
    let seeds, mappings = parseInput input
    let result = findLowestDestination mappings seeds
    result


let solvePart2 (input: string[]) =
    let seeds, mappings = parseInputPart2 input
    findLowestDestination mappings seeds

solvePart1 input |> printfn "Part 1: %i"
let part2Result = solvePart2 input
part2Result |> printfn "Part 2: %i"