module AdventOfCode2023.Day11

open System.Collections.Generic



type Coordinate = { X: int; Y: int }
type Galaxy = { Id: int; Coordinate: Coordinate }

type NightSkyMap =
    { Galaxies: Galaxy[]
      Width: int
      Height: int }

let doubleRows (input: char[][]) =
    input
    |> Array.collect (fun (line: char[]) ->
        let lineLength = line.Length
        let emptyLine = Array.create lineLength '.'

        if emptyLine = line then
            [| line; emptyLine |]
        else
            [| line |])

let rotate (input: char[][]) =
    let height = input.Length
    let width = input[0].Length

    let transposed =
        Array.init width (fun i -> Array.init height (fun j -> input[j][i]))

    Array.map Array.rev transposed


let applyGravityLens (input: string[]) =
    let chars = input |> Array.map (fun line -> line.ToCharArray())

    chars
    |> doubleRows
    |> rotate
    |> doubleRows
    |> rotate
    |> rotate
    |> rotate
    |> Array.map (fun x -> System.String(x))

let mapGalaxies (input: string[]) =
    let chars = input |> Array.map (fun line -> line.ToCharArray())


    chars
    |> Array.mapi (fun y line ->
        line
        |> Array.mapi (fun x c -> if c = '#' then Some(x, y) else None)
        |> Array.choose id)
    |> Array.concat
    |> Array.mapi (fun i (x, y) ->
        { Id = i + 1
          Coordinate = { X = x; Y = y } })

let createNightSkyMap (input: string[]) =
    let galaxies = mapGalaxies input

    { Width = input[0].Length
      Height = input.Length
      Galaxies = galaxies }

let isValidMove rows columns (point: Coordinate) (visited: bool[][]) =
    point.X >= 0
    && point.X < columns
    && point.Y >= 0
    && point.Y < rows
    && not (visited[point.Y][point.X])

let printVisited target (visited: bool[][]) =
    let visitedToString row col v =
        if row = target.X && col = target.Y then "T"
        else if v then "1"
        else "0"

    System.Console.Clear()

    for row in [ 0 .. visited.Length - 1 ] do
        for col in [ 0 .. visited[0].Length - 1 ] do
            let status = visited[row][col] |> visitedToString row col
            printf $"{status}"

        printfn ""

let rec bfs
    (targets: Set<Coordinate>)
    (visited: bool[][])
    (queue: Queue<Coordinate * int>)
    (distances: Map<Coordinate, int>)
    map
    =
    match queue.Count with
    | 0 -> distances
    | _ ->
        let point, distance = queue.Dequeue()

        if targets |> Set.isEmpty then
            distances
        else
            let newPoints =
                [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
                |> List.map (fun (dx, dy) ->
                    { X = point.X + dx; Y = point.Y + dy })
                |> List.filter (fun newPoint ->
                    isValidMove map.Height map.Width newPoint visited)

            newPoints
            |> List.iter (fun newPoint ->
                visited[newPoint.Y][newPoint.X] <- true)

            let pointsToQueue =
                newPoints |> List.map (fun newPoint -> (newPoint, distance + 1))

            let pointsToQueueContainsTargets =
                pointsToQueue
                |> List.filter (fun (newPoint, _) ->
                    Set.contains newPoint targets)

            let newDistances =
                distances
                |> Map.toList
                |> List.append pointsToQueueContainsTargets
                |> Map.ofList

            let foundTargets =
                pointsToQueueContainsTargets |> List.map fst |> Set.ofList

            let newTargets = targets - foundTargets

            match newTargets |> Set.isEmpty with
            | true -> newDistances
            | false ->
                pointsToQueue |> List.iter queue.Enqueue

                bfs targets visited queue newDistances map

let bfsStart
    (map: NightSkyMap)
    (start: Coordinate)
    (galaxies: Set<Coordinate>)
    =
    let visited = Array.init map.Height (fun _ -> Array.create map.Width false)
    visited[start.Y][start.X] <- true
    let queue = Queue<Coordinate * int>()
    queue.Enqueue(start, 0)


    bfs galaxies visited queue Map.empty map

let calculateShortestPathLengths (map: NightSkyMap) =
    let galaxies = map.Galaxies |> Array.map _.Coordinate |> set

    printfn $"Found {galaxies.Count} galaxies"

    let distances =
        map.Galaxies
        |> Array.fold
            (fun state g ->
                printfn
                    $"Calculating distances from {g.Coordinate.X}, {g.Coordinate.Y} galaxy id {g.Id}/{galaxies.Count}"

                let knownDistances =
                    state
                    |> Map.filter (fun (_, toCoordinate) _ ->
                        toCoordinate = g.Coordinate)
                    |> Map.keys
                    |> Seq.map fst
                    |> Set.ofSeq

                let targets =
                    Set.filter ((<>) g.Coordinate) galaxies
                    |> (fun gx -> gx - knownDistances)

                let distances = bfsStart map g.Coordinate targets

                distances
                |> Map.fold
                    (fun acc key value ->
                        Map.add (g.Coordinate, key) value acc)
                    state)
            Map.empty<Coordinate * Coordinate, int>

    distances |> Map.values |> Seq.sum

let solvePart1 (input: string[]) =
    let map = input |> applyGravityLens |> createNightSkyMap
    calculateShortestPathLengths map