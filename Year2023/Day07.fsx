#if INTERACTIVE 
#load "Utils.fsx"
open Utils
#else
module Year2023.Day07
open Year2023.Utils
#endif

// Don't care about "Incomplete pattern matches" warning.
#nowarn "0025"

let puzzle = ("2023", "07")
let input = getInput puzzle

(****************************************************************************
 ********************************** Part 1 **********************************
 ****************************************************************************)

let ex = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

// Order is important! Ensures that Ace > Jack, Queen > N 3, etc
type Card = 
    | N of int
    | Jack
    | Queen
    | King
    | Ace

type HandType = 
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

type HandAndBid = { Hand: Card list; Bid: int; Type: HandType  }

let parseCard = function 
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jack
    | 'T' -> N 10
    | n -> N (charToInt n)

let parseHand (hand: string) = 
    Seq.toList hand
    |> List.map parseCard

let hasNCopies n ls e = 
    n = List.length (List.filter ((=) e) ls)

let handType (hand: Card list) = 
    let ds = List.distinct hand

    match List.length ds with 
    | 1 -> FiveOfAKind
    | 2 when List.exists (hasNCopies 4 hand) ds -> FourOfAKind
    | 2 -> FullHouse
    | 3 when List.exists (hasNCopies 3 hand) ds -> ThreeOfAKind
    | 3 -> TwoPair
    | 4 -> OnePair 
    | 5 -> HighCard 

let parseLine (line: string) = 
    let handAndBid = line.Split(' ')
    let hand = parseHand <| handAndBid.[0]
    let bid = handAndBid.[1]

    { Hand = hand; Bid = int bid ; Type = handType hand}

let parse (inputLines: string seq) = 
    Seq.map parseLine inputLines

let rec compareCards h1 h2 = 
    match h1, h2 with 
    | [], [] -> 0
    | c1 :: h1rest, c2 :: h2rest -> 
        match compare c1 c2 with 
        | 0 -> compareCards h1rest h2rest
        | n -> n

let sortCompare h1 h2 = 
    match compare h1.Type h2.Type with
    | 0 -> compareCards h1.Hand h2.Hand
    | n -> n

let part1 () =
    // ex.Split('\n')
    readLines input
    |> parse
    |> Seq.sortWith sortCompare
    |> Seq.mapi (fun i h -> (h.Bid, i + 1))
    |> Seq.fold (fun sum (b, i) -> sum + (b * i)) 0

(****************************************************************************
 ********************************** Part 2 **********************************
 ****************************************************************************)

type Card2 = 
    | Joker
    | N of int
    | Queen
    | King
    | Ace

type HandAndBid2 = { Hand2: Card2 list; Bid2: int; Type2: HandType }

let parseCard2 = function 
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Joker
    | 'T' -> N 10
    | n -> N (charToInt n)

let parseHand2 (hand: string) = 
    Seq.toList hand
    |> List.map parseCard2

let handType2 (hand: Card2 list) = 
    let ds = List.distinct hand
    let nj = List.length (List.filter ((=) Joker) hand)

    match List.length ds, nj with 
    | 1, _ -> FiveOfAKind
    | 2, n when n > 0 -> FiveOfAKind

    | 2, _ when List.exists (hasNCopies 4 hand) ds -> FourOfAKind
    | 3, 3 -> FourOfAKind
    | 3, 2 -> FourOfAKind
    | 3, 1 when List.exists (hasNCopies 3 hand) ds -> FourOfAKind

    | 3, 1
    | 2, _ -> FullHouse

    | 4, 1 -> ThreeOfAKind
    | 4, 2 -> ThreeOfAKind
    | 3, _ when List.exists (hasNCopies 3 hand) ds -> ThreeOfAKind

    | 3, _ -> TwoPair

    | 5, 1
    | 4, 0 -> OnePair 

    | 5, 0 -> HighCard 

    | d, n -> failwith $"{d}, {n}"

let parseLine2 (line: string) = 
    let handAndBid = line.Split(' ')
    let hand = parseHand2 <| handAndBid.[0]
    let bid = handAndBid.[1]

    { Hand2 = hand; Bid2 = int bid ; Type2 = handType2 hand }

let parse2 (inputLines: string seq) = 
    Seq.map parseLine2 inputLines

let rec compareCards2 h1 h2 = 
    match h1, h2 with 
    | [], [] -> 0
    | c1 :: h1rest, c2 :: h2rest -> 
        match compare c1 c2 with 
        | 0 -> compareCards h1rest h2rest
        | n -> n

let sortCompare2 h1 h2 = 
    match compare h1.Type2 h2.Type2 with
    | 0 -> compareCards2 h1.Hand2 h2.Hand2
    | n -> n

let part2 () = 
    // ex.Split('\n')
    readLines input
    |> parse2
    |> Seq.sortWith sortCompare2
    // |> Seq.iter (printfn "%A")
    |> Seq.mapi (fun i h -> (h.Bid2, i + 1))
    |> Seq.fold (fun sum (b, i) -> sum + (b * i)) 0
