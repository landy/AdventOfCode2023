module AdventOfCode2023.Day4

open System

let testInput = [|
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
|]


let calculateCard (row: string) =
    let numbers = row.Substring(row.IndexOf(':') + 1)
    let numbers = numbers.Split('|', StringSplitOptions.TrimEntries)

    let winningNumbers =
        numbers[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Set

    let cardNumbers =
        numbers[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Set

    Set.intersect winningNumbers cardNumbers |> Set.count


let calculateCardScore (winningNumbers: int) =
    Math.Pow(2.0, float (winningNumbers - 1)) |> int

let part1 (input: string[]) =
    input |> Array.map (calculateCard >> calculateCardScore) |> Array.sum

let calculateAllWinningCounts (input: string[]) =
    input |> Array.map calculateCard

let calculateTotalCountOfCards (cards: int[]) =
    let counts = Array.init cards.Length (fun _ -> 1)

    for index in [ 0 .. cards.Length - 1 ] do
        let winning = cards[index]
        let count = counts[index]

        for j in [ 1..winning ] do
            let updateIndex = index + j

            if updateIndex < cards.Length then
                counts[updateIndex] <- counts[updateIndex] + count

    counts

let part2 (input: string[]) =
    let winningCounts = calculateAllWinningCounts input
    calculateTotalCountOfCards winningCounts |> Array.sum

let input = loadInputFile 4
part1 testInput |> printfn "Test 1: %A"
part2 testInput |> printfn "Test 2: %A"
input |> part1 |> printfn "Part 1: %A"
input |> part2 |> printfn "Part 2: %A"