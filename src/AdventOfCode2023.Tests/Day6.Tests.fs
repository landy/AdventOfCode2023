module AdventOfCode2023.Tests.Day6_Tests

open AdventOfCode2023.Day6
open NUnit.Framework
open FsUnit


[<Test>]
let ``slow speed loses the race`` () =
    let holdDuration = 0
    let neededDistance = 9
    let maximumDuration = 7

    let actual = calculateRaceResult holdDuration neededDistance maximumDuration

    actual |> should equal false

[<Test>]
let ``fast speed wins the race`` () =
    let holdDuration = 2
    let neededDistance = 9
    let maximumDuration = 7

    let actual = calculateRaceResult holdDuration neededDistance maximumDuration

    actual |> should equal true

[<Test>]
let ``calculate correct number of winning scenarios`` () =
    let neededDistance = 9
    let maximumDuration = 7

    let actual = calculateWinningScenarios neededDistance maximumDuration

    actual |> should equal 4

[<Test>]
let ``parse values`` () =
    let input = "Time:      7  15   30"

    let actual = parseValues input

    actual
    |> should equal [|
        7
        15
        30
    |]


[<Test>]
let ``parse inputs`` () =
    let input = [|
        "Time:      7  15   30"
        "Distance:  9  40  200"
    |]

    let actual = parseInput input

    actual
    |> should equal [|
        7, 9
        15, 40
        30, 200
    |]

[<Test>]
let ``parse part2 input`` () =
    let input = "Time:      7  15   30"

    let actual = parseValuesPart2 input

    actual |> should equal 71530


[<Test>]
let ``calculate part 2`` () =
    let input = [|
        "Time:      7  15   30"
        "Distance:  9  40  200"
    |]

    let expected = 71503

    let actual = calculatePart2Results input

    actual |> should equal expected