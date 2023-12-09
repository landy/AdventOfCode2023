module AdventOfCode2023.Tests.Day8_Tests

open FsUnit
open NUnit.Framework
open AdventOfCode2023.Day8


[<Test>]
// [<Ignore("Final test")>]
let ``solve part 1`` () =
    let input = [|
        "RL"
        ""
        "AAA = (BBB, CCC)"
        "BBB = (DDD, EEE)"
        "CCC = (ZZZ, GGG)"
        "DDD = (DDD, DDD)"
        "EEE = (EEE, EEE)"
        "GGG = (GGG, GGG)"
        "ZZZ = (ZZZ, ZZZ)"
    |]

    let actual = solvePart1 input |> fst

    actual |> should equal 2

[<Test>]
let ``parse instructions`` () =
    let instructions = "RL"

    let actual = parseDirections instructions

    actual
    |> should equal [|
        Right
        Left
    |]

[<Test>]
let ``parse coordinates`` () =
    let coordinates = [| "AAA = (BBB, CCC)" |]

    let actual = parseCoordinates coordinates
    let expected = [| "AAA", { Left = "BBB"; Right = "CCC" } |] |> Map.ofArray

    actual |> should equal expected

[<Test>]
let ``find path`` () =
    let directionsMap =
        [| "AAA", { Left = "BBB"; Right = "CCC" } |] |> Map.ofArray

    let strategy = [| Left |]

    let actual = findPath directionsMap strategy "AAA" "BBB" |> fst

    actual |> should equal 1

[<Test>]
// [<Ignore("infinite")>]
let ``part 2`` () =
    let input = [|
        "LR"
        ""
        "11A = (11B, XXX)"
        "11B = (XXX, 11Z)"
        "11Z = (11B, XXX)"
        "22A = (22B, XXX)"
        "22B = (22C, 22C)"
        "22C = (22Z, 22Z)"
        "22Z = (22B, 22B)"
        "XXX = (XXX, XXX)"
    |]

    let actual = solvePart2 input

    actual |> should equal 6