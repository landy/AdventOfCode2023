[<AutoOpen>]
module AdventOfCode2023.Helpers

open System.IO

let loadInputFile (day: int) =
    let path =
        Path.Combine(Directory.GetCurrentDirectory(), "Inputs", $"day{day}.txt")

    File.ReadAllLines(path)

let isDigit (c: char) = System.Char.IsDigit(c)

let charToInt (c: char) = int (c) - int ('0')
