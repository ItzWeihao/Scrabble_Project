namespace CatSquish

open System.Text.RegularExpressions
open CatSquish.MultiSet
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

    let parseMove ts (board: Map<coord, (char * int)>) (hand: MultiSet.MultiSet<uint32>) cstream =
        let play = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"
        let pass = @"pass[ ]?"
        let change = @"change[ ]?"
        
        match ts with
        | Regex play _ -> send cstream (SMPlay (regexMove ts play))
        | Regex change _ -> send cstream (SMChange (toList hand |> Seq.take (int (toList hand |> List.length)) |> Seq.toList))
        | Regex pass _ -> send cstream SMPass

module Print =
    let printHand pieces hand = hand |> fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    type TileMap = Map<uint32, tile>
    
    type CharList = (uint32 * char) list
    
    type WordList = CharList list
    
    type CharPos = coord * (uint32 * (char * int))
    
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
    
    let getMovePieceData (pieces: TileMap) (piece: CharPos): CharPos =
        let coord = fst piece
        let char = fst (snd (snd piece))
        let id = Map.findKey (fun _ (p: tile) -> fst p.MinimumElement = char) (Map.remove 0u pieces)
        let value = snd (pieces.Item(id).MinimumElement)
        (coord, (id, (char, value)))
    
    let getPieceValue (pieces: TileMap) (piece: uint32) : int = snd (pieces.Item(piece).MinimumElement)    
        
    let sortWordsByValue (words: WordList) (pieces: TileMap) : WordList = words |> List.sortByDescending (fun s -> s |> List.fold (fun a v -> a + (getPieceValue pieces (fst v))) 0)
        
    let handToList (hand: MultiSet<uint32>) : CharList = toList hand |> List.fold (fun acc u -> acc @ [(u, char (u + 64u))]) []
    
    let charlistToString (charList: CharList) = List.fold (fun s (_, c) -> s + c.ToString()) "" charList
    
    let layWord (sortedWords: WordList) (pieces: TileMap) (position: CharPos) (isVertical: bool) =
        debugPrint $"sorted words: {sortedWords}\n"
        //debugPrint $"words after sorting:\n"
        //for i in sortedWords do
        //    debugPrint $"{charlistToString i}\n"
        match sortedWords.Length = 0 with
        | true -> "change "
        | false ->
            let firstWord = fst (snd (snd position)) = '*'
            let word = match firstWord with
                       | true -> sortedWords.Head
                       | false -> sortedWords.Head.Tail
            let startPos = fst position
            let mutable i = match isVertical with
                            | true -> if firstWord then startPos else (fst startPos, snd startPos+1)
                            | false -> if firstWord then startPos else (fst startPos+1, snd startPos)
            let layStr = (List.fold (fun s (v, c) ->
                let lay = s + $"{fst i} {snd i} {v}{c}{getPieceValue pieces v} "
                i <- match isVertical with | true -> (fst i, snd i+1) | false -> (fst i+1, snd i)
                lay
                ) "" word)
            layStr
    
    let lookupWords (hand: MultiSet<uint32>) (dict: Dict) (startChar: CharPos) : WordList =
        let firstWord = match fst (snd (snd startChar)) <> '*' with | true -> [fst (snd startChar), fst (snd (snd startChar))] | false -> []
        let rec rackLookup (charList: (uint32 * char) list) (wordCharList: (uint32 * char) list) =
            List.fold (fun acc currChar ->
                    let currWord = match lookup (charlistToString wordCharList) dict with | true -> [wordCharList] | false -> []
                    acc @ currWord @ rackLookup (List.filter ((<>) currChar) charList) (wordCharList @ [currChar])
                    ) [] charList
        let wordPermutations = (match contains 0u hand with
                                | true -> List.fold (fun acc l ->
                                    let currHand = handToList hand |> List.map (fun (i, c) -> if i = 0u then i, (char ((uint32 c)+l)) else i, c)
                                    acc @ rackLookup (currHand @ firstWord) []) [] [0u .. 25u]
                                | false -> rackLookup ((handToList hand) @ firstWord) []) |> Seq.distinct |> List.ofSeq
        //debugPrint $"words before sorting:\n"
        //for i in wordPermutations do
        //    debugPrint $"{charlistToString i}\n"
        debugPrint $"sort first char: {fst (snd (snd startChar))}\n"
        match fst (snd (snd startChar)) <> '*' with
        | true -> List.filter (fun w -> debugPrint $"word: {charlistToString w}, item: {w.Item(0)}, start char: {startChar}, wildcard: {(fst (w.Item(0))) = 0u}, word: {w}, accepted: {(snd (w.Item(0)) = fst (snd (snd startChar))) && ((fst (w.Item(0))) <> 0u)}\n"
                                        (snd (w.Item(0)) = fst (snd (snd startChar))) && ((fst (w.Item(0))) <> 0u)) wordPermutations
        | false -> wordPermutations

module Scrabble =
    let rec playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            Print.printHand pieces st.hand
            
            debugPrint $"last char: {st.wordPlacement}\n"
            
            let words    = State.lookupWords st.hand st.dict st.wordPlacement
            let sorted   = State.sortWordsByValue words pieces
            
            debugPrint $"pieces: {(st.boardState.Count + (toList st.hand).Length)}, board: {st.boardState.Count}, hand: {(toList st.hand).Length}\n"
            
            match ((104 - st.boardState.Count - (toList st.hand).Length) < 7) && sorted.Length = 0 with
            | false -> let layWord  = State.layWord sorted pieces st.wordPlacement st.wordDirection
                       RegEx.parseMove layWord st.boardState st.hand cstream
            | true ->  RegEx.parseMove "pass " st.boardState st.hand cstream
            
            match (recv cstream) with
            | RCM (CMPlayed (_, move, _)) ->
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let addWord = State.getMovePieceData pieces (move.Item(move.Length-1))
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard addWord st.wordDirection
                aux st'
            | RCM (CMPlaySuccess (move, _, newPieces)) ->
                let removePiece = List.fold (fun acc elm -> removeSingle (fst(snd(elm))) acc) st.hand move
                let addPiece = List.fold (fun acc elm -> add (fst elm) (snd elm) acc) removePiece newPieces
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let addWord = State.getMovePieceData pieces (move.Item(move.Length-1))
                debugPrint $"piece: {move.Item(move.Length-1)}, cleanup: ${addWord}\n"
                let wordDir = not st.wordDirection
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer addPiece newBoard addWord wordDir
                aux st'
            | RCM (CMPlayFailed (_, move)) ->
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard st.wordPlacement st.wordDirection)
            | RCM (CMPassed _) ->
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand st.boardState st.wordPlacement st.wordDirection)
            | RCM (CMChangeSuccess newPieces) ->
                let addPiece = List.fold (fun acc elm -> add (fst elm) (snd elm) acc) empty newPieces
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer addPiece st.boardState st.wordPlacement st.wordDirection
                aux st'
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