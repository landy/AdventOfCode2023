#load "Helpers.fsx"
#time "on"

open System
open Helpers

let testInput = [|
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
|]

let input = loadInputFile 3

[<Struct>]
type Coordinate = { X: int; Y: int }

type EnginePart = {
    Value: int
    Coordinates: Coordinate[]
}

type Gear = {
    Coordinate: Coordinate
    Part: EnginePart * EnginePart
}

type Symbol = { IsGear: bool; Coordinate: Coordinate }


type EngineSchema = {
    Symbols: Symbol[]
    Parts: EnginePart[]
}

let emptyEngineSchema = { Symbols = [||]; Parts = [||] }

let isSymbol (c: char) = c <> '.' && not (isDigit c)

let rec parseRowInner
    rowIndex
    columnIndex
    (schema: EngineSchema)
    (row: char[])
    =
    let firstChar = row[0]

    let schema', rest, nextColumnIndex =
        if firstChar |> isSymbol then
            let isGear = firstChar = '*'

            let symbol = {
                IsGear = isGear
                Coordinate = { X = columnIndex; Y = rowIndex }
            }

            {
                schema with
                    Symbols = Array.append schema.Symbols [| symbol |]
            },
            row[1..],
            columnIndex + 1

        else if firstChar |> isDigit then
            let digits = row |> Array.takeWhile isDigit

            let coordinates =
                digits
                |> Array.mapi (fun i _ -> { X = columnIndex + i; Y = rowIndex })

            let partNumber =
                digits
                |> Array.rev
                |> Array.mapi (fun i c ->
                    let digit = c |> charToInt
                    digit * (int32 (Math.Pow(10.0, float i))))
                |> Array.sum

            let rest = row[digits.Length ..]

            {
                schema with
                    Parts =
                        Array.append schema.Parts [|
                            {
                                Value = partNumber
                                Coordinates = coordinates
                            }
                        |]
            },
            rest,
            columnIndex + digits.Length
        else
            let dots = row |> Array.takeWhile (fun c -> c = '.')
            let rest = row[dots.Length ..]
            let nextColumnIndex = columnIndex + dots.Length

            schema, rest, nextColumnIndex

    if rest.Length > 0 then

        parseRowInner rowIndex nextColumnIndex schema' rest
    else
        schema'

let getNeighbourCoordinates (coordinate: Coordinate) = [|
    // LEFT
    {
        X = coordinate.X - 1
        Y = coordinate.Y
    }
    // RIGHT
    {
        X = coordinate.X + 1
        Y = coordinate.Y
    }
    // Bottom
    {
        X = coordinate.X
        Y = coordinate.Y - 1
    }
    // Bottom left
    {
        X = coordinate.X - 1
        Y = coordinate.Y + 1
    }
    // Bottom right
    {
        X = coordinate.X + 1
        Y = coordinate.Y - 1
    }
    // Top
    {
        X = coordinate.X
        Y = coordinate.Y + 1
    }
    // TOP LEFT
    {
        X = coordinate.X - 1
        Y = coordinate.Y - 1
    }
    // TOP RIGHT
    {
        X = coordinate.X + 1
        Y = coordinate.Y + 1
    }
|]

let getPartNeighbours (part: EnginePart) =
    // let partCoordinates = part.Coordinates |> Set.ofArray

    let neighbours =
        part.Coordinates
        |> Array.map getNeighbourCoordinates
        |> Array.concat
        |> Array.filter (fun c -> c.X >= 0 && c.Y >= 0)
        |> Set.ofArray

    // Set.difference neighbours partCoordinates
    neighbours

let areNeighbours (coordinate: Coordinate) (part: EnginePart) =
    let neighbours = getPartNeighbours part
    Set.contains coordinate neighbours

let getNeighbourSymbolCoordinates (symbols: Symbol[]) (part: EnginePart) =
    let possibleCoordinates = getPartNeighbours part

    let symbolsCoordinatesSet =
        symbols |> Array.map (fun x -> x.Coordinate) |> Set.ofArray

    Set.intersect possibleCoordinates symbolsCoordinatesSet

let parseEngineSchema (schema: string[]) =
    let parsedSchema =
        schema
        |> Array.mapi (fun rowIndex row ->
            row.ToCharArray() |> parseRowInner rowIndex 0 emptyEngineSchema)

    parsedSchema
    |> Array.reduce (fun a b -> {
        Symbols = Array.append a.Symbols b.Symbols
        Parts = Array.append a.Parts b.Parts
    })


let filterParts (parsedSchema: EngineSchema) =
    let partsWithNeighbours =
        parsedSchema.Parts
        |> Array.filter (fun parts ->
            getNeighbourSymbolCoordinates parsedSchema.Symbols parts
            |> Set.count > 0)


    {
        parsedSchema with
            Parts = partsWithNeighbours
    }

let getGearParts (schema: EngineSchema) =
    schema.Symbols |> Array.filter _.IsGear

let getGears (coordinate: Coordinate) (parts: EnginePart[]) =
    let neighbourParts =
        parts
        |> Array.mapi (fun i p -> i, areNeighbours coordinate p)
        |> Array.filter snd
        |> Array.map fst
        |> Array.map (fun i -> parts[i])

    if neighbourParts.Length = 2 then
        {
            Coordinate = coordinate
            Part = neighbourParts[0], neighbourParts[1]
        }
        |> Array.singleton
    else
        [||]

let gearRatio (gear: Gear) =
    let part1, part2 = gear.Part
    part1.Value * part2.Value

let calculateGears (schema: EngineSchema) =
    let gearParts = getGearParts schema

    let gears =
        gearParts
        |> Array.map (fun x -> getGears x.Coordinate schema.Parts)
        |> Array.concat

    gears |> Array.map gearRatio |> Array.sum

let schema = parseEngineSchema input
let part1Result = schema |> filterParts |> _.Parts |> Array.sumBy _.Value

let part2Result = schema |> calculateGears