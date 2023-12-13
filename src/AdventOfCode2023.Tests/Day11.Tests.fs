module AdventOfCode2023.Tests.Day11_Tests

open NUnit.Framework
open FsUnit

open AdventOfCode2023.Day11

[<Test>]
let ``empty row expands to double`` () =
    let input = [| ".#" |]

    let expected = [| "..#" |]

    let actual = input |> applyGravityLens

    actual |> should equal expected

[<Test>]
let ``Create night sky map`` () =
    let input = [| ".#..#"; "....."; "....#"; "...##" |]

    let expected =
        { Width = 5
          Height = 4
          Galaxies =
            [| { Id = 1
                 Coordinate = { X = 1; Y = 0 } }
               { Id = 2
                 Coordinate = { X = 4; Y = 0 } }
               { Id = 3
                 Coordinate = { X = 4; Y = 2 } }
               { Id = 4
                 Coordinate = { X = 3; Y = 3 } }
               { Id = 5
                 Coordinate = { X = 4; Y = 3 } } |] }

    let actual = input |> createNightSkyMap

    actual |> should equal expected

[<Test>]
let ``calculate shortest path lengths`` () =
    let input =
        { Width = 13
          Height = 20
          Galaxies =
            [| { Id = 1
                 Coordinate = { X = 0; Y = 4 } }
               { Id = 2
                 Coordinate = { X = 10; Y = 9 } }
               { Id = 3
                 Coordinate = { X = 1; Y = 9 } } |] }


    let actual = input |> calculateShortestPathLengths

    actual |> should equal (6 + 15 + 9)

[<Test>]
let ``test part 1`` () =
    let input =
        [| "...#......"
           ".......#.."
           "#........."
           ".........."
           "......#..."
           ".#........"
           ".........#"
           ".........."
           ".......#.."
           "#...#....." |]

    let expected = 374
    let actual = input |> solvePart1

    actual |> should equal expected