module AdventOfCode2023.Day2

open System

let testInput = [|
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
|]

let input = loadInputFile 2

type DiceColor =
    | Red
    | Green
    | Blue

type Dice = { Color: DiceColor; Count: int }

type Round = Dice[]

type Game = { Id: int; Rounds: Round[] }

let parseGameId (game: string) =
    let colonIndex = game.IndexOf(':')
    game[5 .. (colonIndex - 1)] |> int

let splitRounds (game: string) =
    let colonIndex = game.IndexOf(':')

    game[(colonIndex + 2) ..].Split(';', StringSplitOptions.TrimEntries)
    |> Array.map _.Split(',', StringSplitOptions.TrimEntries)

let parseColor (count: int) (color: string) =
    match color with
    | "red" -> { Color = DiceColor.Red; Count = count } |> Some
    | "green" ->
        {
            Color = DiceColor.Green
            Count = count
        }
        |> Some
    | "blue" ->
        {
            Color = DiceColor.Blue
            Count = count
        }
        |> Some
    | _ -> None

let parseDice (round: string) =
    let dice = round.Split(' ')
    let count = int (dice[0].Trim())
    dice[1] |> parseColor count

let parseGame (game: string) =
    let gameId = parseGameId game

    let rounds =
        splitRounds game
        |> Array.map (fun round ->
            round |> Array.map parseDice |> Array.choose id)

    { Id = gameId; Rounds = rounds }

let isInvalidGame (game: Game) =
    game.Rounds
    |> Array.exists (fun round ->
        round
        |> Array.exists (fun dice ->
            match dice with
            | { Color = DiceColor.Red } -> dice.Count > 12
            | { Color = DiceColor.Green } -> dice.Count > 13
            | { Color = DiceColor.Blue } -> dice.Count > 14))

let isValidGame = not << isInvalidGame

let countMinimums (game: Game) =
    game.Rounds
    |> Array.collect id
    |> Array.groupBy _.Color
    |> Array.map (fun (_, dices) ->
        dices |> Array.maxBy (fun dice -> dice.Count))

let calculatePower (dices: Dice[]) =
    dices |> Array.map _.Count |> Array.reduce (*)

let result1 =
    input |> Array.map parseGame |> Array.filter isValidGame |> Array.sumBy _.Id

let result2 =
    input
    |> Array.map parseGame
    |> Array.map countMinimums
    |> Array.map calculatePower
    |> Array.sumBy id
