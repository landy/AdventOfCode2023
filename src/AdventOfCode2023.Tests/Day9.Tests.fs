module AdventOfCode2023.Tests.Day9_Tests

open NUnit.Framework
open FsUnit
open AdventOfCode2023.Day9

[<Test>]
let ``calculate next lines`` () =
    let input = [|
        1
        2
        3
    |]

    let expected = [|
        [|
            1
            2
            3
        |]
        [|
            1
            1
        |]
        [| 0 |]
    |]

    input |> calculateNextLines |> should equal expected

[<Test>]
let ``is all zeroes returns true for all zeros`` () =
    let input = [|
        0
        0
        0
    |]

    input |> lineIsNotAllZeros |> should equal false

[<Test>]
let ``is all zeroes returns false for not all zeros`` () =
    let input = [|
        0
        1
        0
    |]

    input |> lineIsNotAllZeros |> should equal true

[<Test>]
let ``calculate new last line item`` () =
    let input = [|
        [|
            0
            3
            6
            9
            12
            15
        |]
        [|
            3
            3
            3
            3
            3
        |]
        [| 0 |]
    |]

    let expected = 18

    input |> calculateNewLastItem |> should equal expected

[<Test>]
let ``calculate new first item`` () =
    let input = [|
        [|
            10
            13
            16
            21
            30
            45
        |]
        [|
            3
            3
            5
            9
            15
        |]
        [|
            0
            2
            4
            6
        |]
        [|
            2
            2
            2
        |]
        [|
            0
            0
        |]
    |]

    let expected = 5

    input |> calculateNewFirstItem |> should equal expected

[<Test>]
let ``solve part 1`` () =
    let input = [|
        "0 3 6 9 12 15"
        "1 3 6 10 15 21"
        "10 13 16 21 30 45"
    |]

    let actual = input |> solvePart1

    actual |> should equal 114

[<Test>]
let ``solve part2`` () =
    let input = [|
        "0 3 6 9 12 15"
        "1 3 6 10 15 21"
        "10 13 16 21 30 45"
    |]

    let actual = input |> solvePart2

    actual |> should equal 2