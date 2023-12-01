module Helpers

let loadInputFile (day: int) =
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Inputs", $"day{day}.txt")
    System.IO.File.ReadAllLines(path)