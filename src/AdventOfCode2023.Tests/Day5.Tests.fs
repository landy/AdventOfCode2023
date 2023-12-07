module AdventOfCode2023.Tests.Day5_Tests



open AdventOfCode2023.Day5
open NUnit.Framework
open FsUnit

[<Test>]
let ``returns start position if not in range`` () =
    let startPosition = 3

    let ranges = [|
        {
            SourceRangeStart = 2
            DestinationRangeStart = 0
            RangeLength = 1
        }
    |]

    let result = findDestination ranges startPosition

    result |> should equal startPosition

[<Test>]
let ``returns destination position if in range`` () =
    let startPosition = 2

    let ranges = [|
        {
            SourceRangeStart = 1
            DestinationRangeStart = 10
            RangeLength = 2
        }
    |]

    let result = findDestination ranges startPosition

    result |> should equal 11


[<Test>]
let ``returns lowest destination for seeds`` () =
    let startPositions = [|
        1L
        10L
    |]

    let ranges = [|
        {
            SourceRangeStart = 1
            DestinationRangeStart = 5
            RangeLength = 2
        }
        {
            SourceRangeStart = 10
            DestinationRangeStart = 0
            RangeLength = 2
        }
    |]

    let result = findLowestDestination [| ranges |] startPositions
    result |> should equal 0


[<Test>]
let ``parse seeds`` () =
    let input = [|
        "seeds: 79 14 55 13"
        ""
        "seed-to-soil map:"
        "50 98 2"
        "52 50 48"
    |]

    let result = parseInput input |> fst

    result
    |> should equal [|
        79
        14
        55
        13
    |]

[<Test>]
let ``parse seeds for part 2`` () =
    let input = "seeds: 1 2 10 2"

    let result = parseSeedsPart2 input

    result
    |> should equal [|
        1L
        2L
        10L
        11L
    |]



[<Test>]
let ``parse mapping`` () =
    let input = [|
        "seeds: 79 14 55 13"
        ""
        "seed-to-soil map:"
        "50 98 2"
        "52 50 48"
    |]

    let result = parseInput input |> snd

    result
    |> should equal [|
        [|
            {
                SourceRangeStart = 98
                DestinationRangeStart = 50
                RangeLength = 2
            }
            {
                SourceRangeStart = 50
                DestinationRangeStart = 52
                RangeLength = 48
            }
        |]
    |]

[<Test>]
let ``parse multiple mappings`` () =
    let input = [|
        "seeds: 79 14 55 13"
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
    |]

    let result = parseInput input |> snd

    result
    |> should equal [|
        [|
            {
                SourceRangeStart = 98
                DestinationRangeStart = 50
                RangeLength = 2
            }
            {
                SourceRangeStart = 50
                DestinationRangeStart = 52
                RangeLength = 48
            }
        |]
        [|
            {
                SourceRangeStart = 15
                DestinationRangeStart = 0
                RangeLength = 37
            }
            {
                SourceRangeStart = 52
                DestinationRangeStart = 37
                RangeLength = 2
            }
            {
                SourceRangeStart = 0
                DestinationRangeStart = 39
                RangeLength = 15
            }
        |]
        [|
            {
                SourceRangeStart = 53
                DestinationRangeStart = 49
                RangeLength = 8
            }
            {
                SourceRangeStart = 11
                DestinationRangeStart = 0
                RangeLength = 42
            }
            {
                SourceRangeStart = 0
                DestinationRangeStart = 42
                RangeLength = 7
            }
            {
                SourceRangeStart = 7
                DestinationRangeStart = 57
                RangeLength = 4
            }
        |]
    |]

[<Test>]
let ``calculate path`` () =
    let mappings = [|
        [|
            {
                SourceRangeStart = 0
                DestinationRangeStart = 10
                RangeLength = 1
            }
        |]
        [|
            {
                SourceRangeStart = 10
                DestinationRangeStart = 20
                RangeLength = 1
            }
        |]
    |]

    let result = calculatePath mappings 0
    result |> should equal 20