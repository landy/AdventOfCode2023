#load "Helpers.fsx"

open Helpers

open System

let input = loadInputFile 1

let isDigit (c: char) = c >= '0' && c <= '9'

let folder (characters: char[]) (character: char) =
    if isDigit character then
        Array.append characters [| character |]
    else
        characters

let arrayToNumber (characters: char[]) =
    let numberArray =
        [| (characters |> Array.head); (characters |> Array.last) |]

    String.Concat(numberArray) |> Int32.Parse

let private parseLine (line: string) =
    Array.fold folder [||] (line |> Array.ofSeq) |> arrayToNumber

let part1 = input |> Array.map parseLine |> Array.sum

let startsWith (parser: string * int) (text: string) =
    let key = fst parser
    let value = snd parser

    let isMatch = text.StartsWith(key, StringComparison.OrdinalIgnoreCase)
    if isMatch then Some(value) else None

let parseTextNumber (text: string) =
    let representations =
        [ "one", 1
          "two", 2
          "three", 3
          "four", 4
          "five", 5
          "six", 6
          "seven", 7
          "eight", 8
          "nine", 9 ]

    let result =
        representations
        |> List.map (fun parser -> startsWith parser text)
        |> List.choose id
        |> List.tryHead

    match result with
    | Some(value) -> Some(value, text.Substring(1))
    | None -> None

let rec parseNumbers (line: string) (digits: int[]) : int[] =
    let first = line[0]

    let tail, numbers =
        if isDigit first then
            let tail = line.Substring(1)
            let numbers = Array.append digits [| (Int32.Parse(string first)) |]
            tail, numbers
        else
            match parseTextNumber line with
            | Some(value, tail) ->
                let numbers = Array.append digits [| value |]
                tail, numbers
            | None ->
                let tail = line.Substring(1)
                tail, digits

    if tail.Length = 0 then
        numbers
    else
        parseNumbers tail numbers

let parseLinePart2 (line: string) =
    let numbers = parseNumbers line [||]
    let first = numbers |> Array.head |> (*) 10
    let last = numbers |> Array.last
    first + last

let part2 = input |> Array.map parseLinePart2 |> Array.sum