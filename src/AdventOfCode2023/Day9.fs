module AdventOfCode2023.Day9

open System

let lineIsNotAllZeros (line: int[]) =
    let counts = line |> Array.countBy id
    let firstKey = counts[0] |> fst
    counts.Length <> 1 || firstKey <> 0

let calculateNextLines (input: int[]) =
    let newLines =
        input
        |> Array.unfold (fun state ->
            if lineIsNotAllZeros state then
                let nextLine =
                    state |> Array.pairwise |> Array.map (fun (x, y) -> y - x)

                Some(nextLine, nextLine)
            else
                None)

    Array.append [| input |] newLines

let calculateNewLastItem (lines: int[][]) =
    lines |> Array.map (fun line -> line[line.Length - 1]) |> Array.reduce (+)

let calculateNewFirstItem (lines: int[][]) =
    let folder state item = item - state

    let items = lines |> Array.map Array.head |> Array.rev

    items |> Array.reduce folder

let calculateNewValueForLinePart1 (line: int[]) =
    line |> calculateNextLines |> calculateNewLastItem

let calculateNewValueForLinePart2 (line: int[]) =
    line |> calculateNextLines |> calculateNewFirstItem

let parseLines (input: string) =
    input.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let solvePart1 (input: string[]) =
    input
    |> Array.map parseLines
    |> Array.map calculateNewValueForLinePart1
    |> Array.sum

let solvePart2 (input: string[]) =
    input
    |> Array.map parseLines
    |> Array.map calculateNewValueForLinePart2
    |> Array.sum