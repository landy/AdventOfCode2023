module AdventOfCode2023.Tests.Day10_Tests

open NUnit.Framework
open FsUnit
open AdventOfCode2023.Day10

[<Test>]
let ``Parse graph`` () =
    let input = [| "-L|F7"; "7S-7|"; "L|7||"; "-L-J|"; "L|-JF" |]

    let expected =
        { Navigation =
            [| 0, [| 1 |]
               1, [| 2 |]
               2, [| 7 |]
               3, [| 8; 4 |]
               4, [| 9; 3 |]

               5, [| 10 |]
               6, [| 7; 11 |]
               7, [| 6; 8 |]
               8, [| 13; 7 |]
               9, [| 4; 14 |]

               10, [| 5; 11 |]
               11, [| 6; 16 |]
               12, [| 17; 11 |]
               13, [| 8; 18 |]
               14, [| 9; 19 |]

               15, [| 16 |]
               16, [| 11; 17 |]
               17, [| 16; 18 |]
               18, [| 13; 17 |]
               19, [| 14; 24 |]

               20, [| 15; 21 |]
               21, [| 16 |]
               22, [| 21; 23 |]
               23, [| 18; 22 |]
               24, [||]

               |]
            |> Map.ofArray
          Start = 6 }

    let actual = parse input
    actual |> should equal expected

[<TestCase(2, 0, 3, 2)>]
[<TestCase(0, 1, 3, 3)>]
[<TestCase(1, 1, 3, 4)>]
let ``flat index`` (column, row, width, expected) =

    let actual = flatIndex width column row

    actual |> should equal expected

[<Test>]
let ``find cycles`` () =
    let input =
        { Navigation =
            [| 0, [| 1; 3 |]
               1, [| 0; 2 |]
               2, [| 5; 1 |]
               3, [| 0; 4 |]
               4, [| 3; 5 |]
               5, [| 2; 4 |] |]
            |> Map.ofArray
          Start = 0 }

    let expected = [| 1; 2; 5; 4; 3 |]

    let actual = findCycles input
    actual |> should equal expected

[<Test>]
let ``solve part 1`` () =
    let input = [| "..F7."; ".FJ|."; "SJ.L7"; "|F--J"; "LJ..." |]

    let actual = solvePart1 input
    actual |> should equal 8