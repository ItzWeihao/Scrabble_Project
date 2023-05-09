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
        
    let regexMove ts pattern =
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

    let parseMove ts cstream =
        let play = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"
        let change = @"change[ ]?"
        
        debugPrint $"\nCurrent move: {ts}\n\n"
        match ts with
        | Regex play _ -> send cstream (SMPlay (regexMove ts play))

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
        wordPlacement   : coord * (uint32 * (char * int))
        wordDirection   : bool
    }
    type TileMap = Map<uint32, tile>
    type Word = char list
    type WordList = (uint32 * char) list list
    type CharPos = coord * (uint32 * (char * int))
    type LayedWordsList = (coord * (bool * string)) list
    type CharIndex = (int * char) Option
    
    let mkState board dict playerNumber numberOfPlayer hand bState wPlace wDir = {
        board = board
        dict = dict
        playerNumber = playerNumber
        numberOfPlayer = numberOfPlayer
        hand = hand
        boardState = bState
        wordPlacement = wPlace
        wordDirection = wDir
    }
    
    let getPieceValue (pieces: TileMap) (piece: uint32) : int =
        snd (pieces.Item(piece).MinimumElement)    
        
    let sortWordsByValue (words: WordList) (pieces: TileMap) : WordList =
        words |> List.sortByDescending (fun s -> s |> List.fold (fun a v -> a + (getPieceValue pieces (fst v))) 0) //|> List.filter (fun w -> w.Length > 2)
        
    let handToList (hand: MultiSet<uint32>) : (uint32 * char) list =
        toList hand |> List.fold (fun acc u -> acc @ [(u, char (u + 64u))]) []
    
    let charlistToString (charList: (uint32 * char) list) =
        List.fold (fun s (_, c) -> s + c.ToString()) "" charList
    
    let layWord (sortedWords: WordList) (pieces: TileMap) (position: CharPos) (isVertical: bool) =
        debugPrint $"position: ${position},\nisVertical: {isVertical},\nstartPos: {fst position},\n word count: {sortedWords.Length}\n"
        debugPrint "words after filter:\n"
        List.iter (fun w -> debugPrint $"{charlistToString w}\n") sortedWords
        let firstWord = fst (snd (snd position)) = '*'
        let word = match firstWord with
                   | true -> sortedWords.Head
                   | false -> sortedWords.Head.Tail
        let startPos = fst position
        let mutable i = match isVertical with
                        | true -> if firstWord then startPos else (fst startPos, snd startPos+1)
                        | false -> if firstWord then startPos else (fst startPos+1, snd startPos)
        debugPrint $"i: {i}\n"
        let layStr = (List.fold (fun s (v, c) ->
            let lay = s + $"{fst i} {snd i} {v}{c}{getPieceValue pieces v} "
            i <- match isVertical with | true -> (fst i, snd i+1) | false -> (fst i+1, snd i)
            lay
            ) "" word)
        debugPrint $"lay string: {layStr}\n"
        layStr
    
    let lookupWords (hand: MultiSet<uint32>) (dict: Dict) (startChar: CharPos) : WordList =
        debugPrint $"wildcard: {contains 0u hand},\nhand: {hand},\nhand string: {charlistToString (handToList hand)},\nhand list {handToList hand},\nstart char: {fst (snd (snd startChar))}\n"
        let firstWord = match fst (snd (snd startChar)) <> '*' with | true -> [(uint32 (snd (fst startChar)), fst (snd (snd startChar)))] | false -> []
        let rec rackLookup (charList: (uint32 * char) list) (wordCharList: (uint32 * char) list) =
            List.fold (fun acc currChar ->
                    let currWord = match lookup (charlistToString wordCharList) dict with | true -> [wordCharList] | false -> []
                    //debugPrint $"rack; charList: {charList}, wordCharList {charlistToString wordCharList}, head: {charList.Head}, currChar: {currChar}\n"
                    acc @ currWord @ rackLookup (List.filter ((<>) currChar) charList) (wordCharList @ [currChar])
                    ) [] charList
        let wordPermutations = (match contains 0u hand with
                                | true -> List.fold (fun acc l ->
                                    let currHand = handToList hand |> List.map (fun (i, c) -> if i = 0u then i, (char ((uint32 c)+l)) else i, c)
                                    acc @ rackLookup (currHand @ firstWord) []) [] [0u .. 25u]
                                | false -> rackLookup ((handToList hand) @ firstWord) []) |> Seq.distinct |> List.ofSeq
        debugPrint "words before filter:\n"
        List.iter (fun w -> debugPrint $"{charlistToString w}\n") wordPermutations
        match fst (snd (snd startChar)) <> '*' with
        | true -> List.filter (fun w -> snd (w.Item(0)) = fst (snd (snd startChar))) wordPermutations
        | false -> wordPermutations

module Scrabble =
    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            debugPrint $"------------------------------------------------------------\n\n"
            Print.printHand pieces st.hand
            
            // Finds words and their position on the board
            let words    = State.lookupWords st.hand st.dict st.wordPlacement
            let sorted   = State.sortWordsByValue words pieces
            if sorted.Length = 0 then send cstream (SMChange (toList st.hand))
            else
                let layWord  = State.layWord sorted pieces st.wordPlacement st.wordDirection
            
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
                let addWord = move.Item(move.Length-1)
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard addWord st.wordDirection
                aux st'
            | RCM (CMPlaySuccess (move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removePiece = List.fold (fun acc elm -> removeSingle (fst(snd(elm))) acc) st.hand move
                let addPiece = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) removePiece newPieces
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let addWord = move.Item(move.Length-1)
                let wordDir = not st.wordDirection
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer addPiece newBoard addWord wordDir
                aux st'
            | RCM (CMPlayFailed (playerId, move)) ->
                (* Failed play. Update your state *)
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard st.wordPlacement st.wordDirection)
            | RCM (CMPassed playerId) ->
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand st.boardState st.wordPlacement st.wordDirection)
            //| RCM (CMForfeit playerId) ->
            //| RCM (CMChange (playerId, numberOfTiles)) ->  
            | RCM (CMChangeSuccess newPieces) ->
                let addPiece = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) empty newPieces
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer addPiece st.boardState st.wordPlacement st.wordDirection
                aux st'
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
        let wordPlacement = ((0,0), (0u, ('*', 0)))
        
        fun () -> playGame cstream tiles (State.mkState boardP dict playerNumber numPlayers handSet Map.empty wordPlacement true)