module AdventOfCode2023.Day7

let part2JokerChar = 'X'

let useJokers (counts: (char * int)[]) : (char * int)[] =
    let jokerCount =
        counts
        |> Map.ofArray
        |> Map.tryFind part2JokerChar
        |> Option.defaultValue 0

    if jokerCount > 0 && jokerCount < 5 then
        let jokerPosition =
            counts |> Array.findIndex (fun (c, _) -> c = part2JokerChar)

        let updateIndex = if jokerPosition = 0 then 1 else 0

        let firstCardCount =
            counts[updateIndex] |> (fun (c, v) -> c, v + jokerCount)

        counts[updateIndex] <- firstCardCount
        counts |> Array.removeAt jokerPosition
    else
        counts


let (|FiveOfKind|FourOfKind|FullHouse|ThreeOfKind|TwoPair|OnePair|HighCard|)
    (hand: char[])
    =
    let counts =
        hand
        |> Array.countBy id
        |> Array.sortByDescending snd
        |> useJokers
        |> Array.map snd

    match counts with
    | [| 5 |] -> FiveOfKind
    | [| 4; 1 |] -> FourOfKind
    | [| 3; 2 |] -> FullHouse
    | [| 3; 1; 1 |] -> ThreeOfKind
    | [| 2; 2; 1 |] -> TwoPair
    | [| 2; 1; 1; 1 |] -> OnePair
    | [| 1; 1; 1; 1; 1 |] -> HighCard
    | _ ->
        let cards = System.String(hand)
        failwith $"Invalid hand of cards {cards} has counts %A{counts}"


let rankHand (hand: char[]) =
    match hand with
    | FiveOfKind -> 7
    | FourOfKind -> 6
    | FullHouse -> 5
    | ThreeOfKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1


let mapCardValue (card: char) =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | '9' -> 9
    | '8' -> 8
    | '7' -> 7
    | '6' -> 6
    | '5' -> 5
    | '4' -> 4
    | '3' -> 3
    | '2' -> 2
    | part2JokerChar -> 1
    | _ -> failwith $"Invalid card {card}"


let compareValues (a: char[]) (b: char[]) =
    let aValues = a |> Array.map mapCardValue
    let bValues = b |> Array.map mapCardValue

    let zipped = Array.zip aValues bValues

    zipped
    |> Array.tryFind (fun (a, b) -> a <> b)
    |> Option.map (fun (a, b) -> compare a b)
    |> Option.defaultValue 0

let compareHands (a: char[]) (b: char[]) =
    let aRank = rankHand a
    let bRank = rankHand b

    if aRank > bRank then 1
    elif aRank < bRank then -1
    else compareValues a b

let compareInputRow (inputA: string * int) (inputB: string * int) =
    let handA = fst inputA |> Seq.toArray
    let handB = fst inputB |> Seq.toArray
    compareHands handA handB

let sortInputs (inputs: (string * int)[]) =
    inputs |> Array.sortWith compareInputRow |> Array.rev

let calculateScore (inputs: (string * int)[]) =
    inputs
    |> sortInputs
    |> Array.rev
    |> Array.map snd
    |> Array.indexed
    |> Array.map (fun (i, hand) -> (i + 1) * hand)
    |> Array.sum

let parseRow (row: string) =
    let parts = row.Split(' ')
    let hand = parts[0]
    let score = int parts[1]
    (hand, score)

let parseRow2 (row: string) =
    let parts = row.Split(' ')
    let hand = parts[0].Replace('J', part2JokerChar)
    let score = int parts[1]
    (hand, score)



let solvePart1 (input: string[]) =
    input |> Array.map parseRow |> calculateScore

let solvePart2 (input: string[]) =
    input |> Array.map parseRow2 |> calculateScore