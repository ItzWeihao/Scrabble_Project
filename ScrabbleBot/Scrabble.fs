namespace CatSquish

open System.Text.RegularExpressions
open CatSquish.MultiSet
open Eval
open Parser
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts cstream =
        let play = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"
        let pass = @"pass[ ]?"
        
        debugPrint $"\nCurrent move: {ts}\n\n"
        match ts with
        | Regex play _ ->
            let rx = (Regex.Matches(ts, play) |>
            Seq.cast<Match> |> 
            Seq.map 
                (fun t -> 
                    match t.Value with
                    | Regex play [x; y; id; c; p] ->
                        ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                    | _ -> failwith "Failed (should never happen)") |>
            Seq.toList)
            send cstream (SMPlay rx)
        | Regex pass _ -> send cstream SMPass

 module Print =
    let printHand pieces hand = hand |> fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    type state = {
        board           : boardProg
        dict            : Dict
        playerNumber    : uint32
        numberOfPlayer  : uint32
        hand            : MultiSet.MultiSet<uint32>
        boardState      : Map<coord, (char * int)>
        layedWords      : (coord * (bool * string)) list
    }
    type TileMap = Map<uint32, tile>
    type Word = char list
    type WordList = string list
    type CharPos = (coord * (bool * char)) Option
    type LayedWordsList = (coord * (bool * string)) list
    type CharIndex = (int * char) Option
    
    let mkState board dict playerNumber numberOfPlayer hand bState lWords = {
        board = board
        dict = dict
        playerNumber = playerNumber
        numberOfPlayer = numberOfPlayer
        hand = hand
        boardState = bState
        layedWords = lWords
    }
    
    let getPieceValue (pieces: TileMap) (piece: char) =
        snd (pieces.Item((uint32 piece)-64u).MinimumElement)
        
    let isWordLayable (words: LayedWordsList) (pos: coord) (isVertical: bool) : bool =
        let i = match isVertical with | true -> (fst pos+1, snd pos) | false -> (fst pos, snd pos+1)
        not (List.exists (fun (c, (v, _)) -> match v with | true -> (fst c+1, snd c) = i | false -> (fst c, snd c+1) = i) words)
    
    let layPosition (words: LayedWordsList) : CharPos =
        match List.tryFind (fun w -> isWordLayable words (fst w) (fst (snd w))) words with
        | Some word -> let coordinate = fst word
                       let isVertical = not (fst (snd word))
                       let character  = (snd (snd word)).Chars(0)
                       Some (coordinate, (isVertical, character))
        | None -> None
        
    let toCharIndex (chars: CharPos) : CharIndex =
        match chars with
        | Some word -> Some (0, snd (snd word))
        | None -> None
    
    let layWord (words: LayedWordsList) (sortedWords: WordList) (pieces: TileMap) (position: CharPos) =
        let word = match words.IsEmpty with | true -> sortedWords.Head.ToCharArray() |> List.ofArray | false -> (sortedWords.Head.ToCharArray() |> List.ofArray).Tail
        let isVertical = match position with | Some pos -> fst (snd pos) | None -> true
        let startPos = match position with | Some pos -> fst pos | None -> (0,0)
        let mutable i = match isVertical with | true -> (fst startPos, -1) | false -> (-1, snd startPos)
        List.fold (fun s v ->
            i <- match isVertical with | true -> (fst i, snd i+1) | false -> (fst i+1, snd i)
            s + $"{fst i} {snd i} {int v-64}{v}{getPieceValue pieces v} "
            ) "" word
    
    let getPieceData (pieces: TileMap) =
        Map.fold (fun s k v -> Map.add k (fst (Set.toList v).Head) s) Map.empty pieces
    
    let sortWordsByValue (words: WordList) (pieces: TileMap) : WordList =
        List.sortByDescending (fun s -> s.ToCharArray() |> List.ofArray |> List.fold (fun a v -> a + (getPieceValue pieces v)) 0) words
    
    let lookupWords (hand: Word) (dict: Dict) (specialChars: CharPos) : WordList =
        let charIndex = toCharIndex specialChars
        let rec rackLookup l w d =
            List.fold (fun s v ->
                    let wordn = match lookup w d with | true -> [w] | false -> []
                    s @ wordn @ (rackLookup (List.filter ((<>) v) l) (w+v.ToString()) d)
                    ) [] l
        let words = rackLookup (hand @ [' ']) "" dict |> Seq.distinct |> List.ofSeq
        match charIndex with
        | Some charIndex -> List.filter (fun w -> w.Chars(fst charIndex) = snd charIndex) words
        | None -> words
    
    let handToList (hand: MultiSet<uint32>) (pieces: TileMap) : Word =
        toList hand |> List.fold (fun s v -> s @ [(getPieceData pieces).Item(v)]) []
        
    let isWordVertical (coord1: coord) (coord2: coord) : bool =
        (snd coord1) <> (snd coord2)
    
    let moveToPair (move: (coord * (uint32 * (char * int))) list) : coord * (bool * string) =
        let word = List.fold (fun s v -> s + (fst (snd (snd v))).ToString()) "" move
        (fst move.Head, (isWordVertical (fst move.Head) (fst (move.Item(1))), word))

module Scrabble =
    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            Print.printHand pieces st.hand
            
            // Finds words and their position on the board
            let position = State.layPosition st.layedWords
            let letters  = State.handToList st.hand pieces
            let words    = State.lookupWords letters st.dict position
            let sorted   = State.sortWordsByValue words pieces
            let layWord  = State.layWord st.layedWords sorted pieces position
            
            // Tries to place a word on the board
            RegEx.parseMove layWord cstream
            
            //let input = System.Console.ReadLine()
            //RegEx.parseMove input cstream
            
            //debugPrint $"Player %d{st.playerNumber} -> Server:\n%A{move}\n"
            //debugPrint $"Player %d{st.playerNumber} <- Server:\n%A{move}\n"
            
            match (recv cstream) with
            | RCM (CMPlayed (playerId, move, points)) ->
                (* Successful play by other player. Update your state *)
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let addWord = st.layedWords @ [State.moveToPair move]
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard addWord
                aux st'
            | RCM (CMPlaySuccess (move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removePiece = List.fold (fun acc elm -> removeSingle (fst(snd(elm))) acc) st.hand move
                let addPiece = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) removePiece newPieces
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let addWord = st.layedWords @ [State.moveToPair move]
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer addPiece newBoard addWord
                aux st'
            | RCM (CMPlayFailed (playerId, move)) ->
                (* Failed play. Update your state *)
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard st.layedWords)
            | RCM (CMPassed playerId) ->
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand st.boardState st.layedWords)
            //| RCM (CMForfeit playerId) ->
            //| RCM (CMChange (playerId, numberOfTiles)) ->  
            //| RCM (CMChangeSuccess newTiles) ->
            //| RCM (CMTimeout playerId) ->
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            $"Starting game!
              number of players = %d{numPlayers}
              player id = %d{playerNumber}
              player turn = %d{playerTurn}
              hand =  %A{hand}
              timeout = %A{timeout}\n\n"

        let dict = dictf false // Set to false for using trie, true for gaddag
        // let board = Parser.mkBoard boardP
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) empty hand
        
        fun () -> playGame cstream tiles (State.mkState boardP dict playerNumber numPlayers handSet Map.empty List.empty)