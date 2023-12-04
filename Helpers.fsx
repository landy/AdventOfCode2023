module Helpers

let loadInputFile (day: int) =
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Inputs", $"day{day}.txt")
    System.IO.File.ReadAllLines(path)

let isDigit (c: char) = System.Char.IsDigit(c)

let charToInt (c: char) = int(c) - int('0')