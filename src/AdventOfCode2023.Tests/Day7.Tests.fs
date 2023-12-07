module AdventOfCode2023.Tests.Day7_Tests

open FsUnit
open NUnit.Framework
open AdventOfCode2023.Day7


[<Test>]
let ``Five of a kind is higher than Four of a kind`` () =
    let fiveOfAKind = "AAAAA".ToCharArray()
    let fourOfAKind = "AA8AA".ToCharArray()

    let actual = compareHands fiveOfAKind fourOfAKind
    actual |> should equal 1

[<Test>]
let ``Four of a kind is higher than Full house`` () =
    let fourOfAKind = "AA8AA".ToCharArray()
    let fullHouse = "23332".ToCharArray()

    let actual = compareHands fourOfAKind fullHouse
    actual |> should equal 1

[<Test>]
let ``Full house is higher than Three of a kind`` () =
    let fullHouse = "23332".ToCharArray()
    let threeOfKind = "TTT98".ToCharArray()

    let actual = compareHands fullHouse threeOfKind
    actual |> should equal 1

[<Test>]
let ``Three of a kind is higher than Two pair`` () =
    let threeOfKind = "TTT98".ToCharArray()
    let twoPair = "23432".ToCharArray()

    let actual = compareHands threeOfKind twoPair
    actual |> should equal 1

[<Test>]
let ``Two pair is higher than High Card`` () =
    let twoPair = "23432".ToCharArray()
    let highCard = "23456".ToCharArray()

    let actual = compareHands twoPair highCard
    actual |> should equal 1

[<TestCase("AAAAA", "KKKKK", 1)>]
[<TestCase("KKKKK", "QQQQQ", 1)>]
[<TestCase("QQQQQ", "JJJJJ", 1)>]
[<TestCase("JJJJJ", "TTTTT", 1)>]
[<TestCase("TTTTT", "99999", 1)>]
[<TestCase("99999", "88888", 1)>]
[<TestCase("88888", "77777", 1)>]
[<TestCase("77777", "66666", 1)>]
[<TestCase("66666", "55555", 1)>]
[<TestCase("55555", "44444", 1)>]
[<TestCase("44444", "33333", 1)>]
[<TestCase("33333", "22222", 1)>]
let ``Higher card wins``
    (
        firstHand: string,
        secondHand: string,
        comparisonValue: int
    ) =
    let aces = firstHand.ToCharArray()
    let kings = secondHand.ToCharArray()

    let actual = compareHands aces kings
    actual |> should equal comparisonValue

[<Test>]
let ``sort hands correctly based on type`` () =
    let hands = [|
        "23456", 0
        "TTT98", 0
        "AA8AA", 0
        "23332", 0
        "23432", 0
        "A23A4", 0
        "AAAAA", 0
    |]

    let expected = [|
        "AAAAA", 0
        "AA8AA", 0
        "23332", 0
        "TTT98", 0
        "23432", 0
        "A23A4", 0
        "23456", 0
    |]

    let actual = hands |> sortInputs
    actual |> should equal expected

[<Test>]
let ``Calculates correct score`` () =
    let inputs = [|
        "32T3K", 765
        "T55J5", 684
        "KK677", 28
        "KTJJT", 220
        "QQQJA", 483
    |]

    let actual = inputs |> calculateScore
    actual |> should equal 6440

[<Test>]
let ``correctly parse row`` () =
    let input = "32T3K 765"
    let expected = "32T3K", 765

    let actual = parseRow input
    actual |> should equal expected

[<Test>]
let ``Solve part 1`` () =
    let input = [|
        "32T3K 765"
        "T55J5 684"
        "KK677 28"
        "KTJJT 220"
        "QQQJA 483"
    |]

    let actual = solvePart1 input
    actual |> should equal 6440

[<TestCase("T55X5", 6)>]
[<TestCase("X37QA", 2)>]
let ``Joker can make the hand more valuable``
    (
        handString: string,
        expectedValue
    ) =
    let hand = handString.ToCharArray()

    let actual = rankHand hand

    actual |> should equal expectedValue

[<Test>]
let ``Solve part 2`` () =
    let input = [|
        "32T3K 765"
        "T55J5 684"
        "KK677 28"
        "KTJJT 220"
        "QQQJA 483"
    |]

    let actual = solvePart2 input

    actual |> should equal 5905