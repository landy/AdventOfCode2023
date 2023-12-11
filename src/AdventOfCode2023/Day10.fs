module AdventOfCode2023.Day10

open Microsoft.FSharp.Core

type Navigation = Map<int, int[]>

type MazeMap = { Navigation: Navigation; Start: int }

let directions node =
    let north = 0, -1
    let south = 0, 1
    let west = -1, 0
    let east = 1, 0

    [| '|', [| north; south |]
       '-', [| west; east |]
       'L', [| north; east |]
       'J', [| north; west |]
       '7', [| south; west |]
       'F', [| south; east |]
       '.', [||]
       'S', [| north; east; south; west |] |]
    |> Map.ofArray
    |> Map.find node

let flatIndex width x y = y * width + x

let findStart (input: string[]) =
    input
    |> Array.indexed
    |> Array.filter (fun (_, line) -> line.Contains('S'))
    |> Array.head
    |> (fun (index, line) -> line.IndexOf('S'), index)

let parse (input: string[]) : MazeMap =
    let gridWidth = input[0].Length
    let gridHeight = input.Length

    let navigation =
        input
        |> Array.map (_.ToCharArray())
        |> Array.mapi (fun rowIndex line ->
            line
            |> Array.mapi (fun colIndex c ->
                let index = flatIndex gridWidth colIndex rowIndex

                let connections =
                    directions c
                    |> Array.map (fun (dx, dy) ->
                        let x = colIndex + dx
                        let y = rowIndex + dy

                        if
                            x < 0
                            || x >= gridWidth
                            || y < 0
                            || y >= gridHeight
                        then
                            None
                        else
                            Some(flatIndex gridWidth x y))
                    |> Array.choose id

                index, connections))
        |> Array.concat
        |> Map.ofArray

    let start = findStart input ||> flatIndex gridWidth

    let startConnections =
        navigation[start]
        |> Array.map (fun x -> x, navigation[x])
        |> Array.filter (fun (x, y) -> y |> Array.contains start)
        |> Array.map fst


    { Navigation = navigation |> Map.add start startConnections
      Start = start }




let rec findPath (mazeMap: MazeMap) currentNode visited =
    let previous = visited |> List.head

    let next =
        mazeMap.Navigation[currentNode]
        |> Array.filter (fun x -> x <> previous)
        |> Array.head

    let visited' = currentNode :: visited

    if next = mazeMap.Start then
        visited'
    else
        findPath mazeMap next visited'


let findCycles (graph: MazeMap) =
    let start = graph.Navigation[graph.Start] |> Array.head

    findPath graph start [ graph.Start ]
    |> List.rev
    |> List.tail
    |> List.toArray

let solvePart1 (input: string[]) =
    let mazeMap = parse input

    let cycles = findCycles mazeMap

    let result =
        if cycles.Length % 2 = 0 then
            cycles.Length / 2
        else
            cycles.Length / 2 + 1

    result