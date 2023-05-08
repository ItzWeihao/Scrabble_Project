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
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    
    type TileMap = Map<uint32, tile>
    type Word = char list
    
    let getPieceValue (pieces: TileMap) (piece: char) =
        snd (pieces.Item((uint32 piece)-64u).MinimumElement)
        
    let isWordLayable (words: (coord * (bool * string)) list) (coord: coord) (isVertical: bool) : bool =
        let i = match isVertical with | true -> (fst coord+1, snd coord) | false -> (fst coord, snd coord+1)
        //for l in board do debugPrint $"coord: {l.Key}, char: {l.Value}\n"
        //not (Map.exists (fun n _ -> debugPrint $"i: {i}, n: {n}\n"; n = i) board)
        not (List.exists (fun (c, (v, s: string)) ->
            List.exists (fun l -> match v with
                         | true -> (fst c, snd c+l) = i
                         | false -> (fst c+l, snd c) = i
                         ) [ 0 .. s.Length ]
            ) words)
    
    let layPosition (words: (coord * (bool * string)) list) : coord * (bool * char) =
        debugPrint $"can lay word: {isWordLayable words (0,0) true}\n"
        let word = List.find (fun w -> debugPrint $"{w}\n"; isWordLayable words (fst w) (fst (snd w))) words
        let coordinate = fst word
        let isVertical = not (fst (snd word))
        let character  = (snd (snd word)).Chars(0)
        debugPrint $"position debug: {word}\n"
        (coordinate, (isVertical, character))
    
    let layWord (pieces: TileMap) (word: Word) (startPos: coord) (isVertical: bool) (cstream: Stream) =
        let mutable i = match isVertical with | true -> (fst startPos, -1) | false -> (-1, snd startPos)
        RegEx.parseMove (List.fold (fun s v ->
            i <- match isVertical with | true -> (fst i, snd i+1) | false -> (fst i+1, snd i)
            s + $"{fst i} {snd i} {int v-64}{v}{getPieceValue pieces v} "
            ) "" word) cstream
        
    let cutWord (board: Map<coord, char * int>) (startPos: coord) (isVertical: bool) (wordList: string list): coord * Word =
        let wordToLay = wordList.Head.ToCharArray() |> List.ofArray
        if board.Count > 0 then
            let wordCut = match board.Count <> 0 with | true -> wordToLay.Tail | false -> wordToLay
            let newCoord = match isVertical with | true -> (fst startPos+1, snd startPos) | false -> (fst startPos, snd startPos+1)
            (newCoord, wordCut)
        else (startPos, wordToLay)
    
    // <(piece index; (piece char; piece value)); ...> --> <(piece index; piece char); ...>
    let getPieceData (pieces: TileMap) =
        Map.fold (fun s k v -> Map.add k (fst (Set.toList v).Head) s) Map.empty pieces
    
    let sortWordsByValue (words: string list) (pieces: TileMap) : string list =
        List.sortByDescending (fun s -> s.ToCharArray() |> List.ofArray |> List.fold (fun a v -> a + (getPieceValue pieces v)) 0) words
    
    let lookupWords (hand: Word) (dict: Dict) : string list =
        let rec rackLookup l w d =
            List.fold (fun s v ->
                    let wordn = if lookup w d then [w] else []
                    s @ wordn @ (rackLookup (List.filter ((<>) v) l) (w+v) d)
                    ) List.empty l
        rackLookup (hand @ [' ']) "" dict |> Seq.distinct |> List.ofSeq
    
    let handToList (hand: MultiSet<uint32>) (pieces: TileMap) : Word =
        toList hand |> List.fold (fun s v -> s @ [(getPieceData pieces).Item(v)]) []
        
    let isWordVertical (coord1: coord) (coord2: coord) : bool =
        (snd coord1) <> (snd coord2)
    
    // [(coord; (piece index; (piece char; piece value)))] --> (coord; (vertical; word))
    let moveToPair (move: (coord * (uint32 * (char * int))) list) : coord * (bool * string) =
        let word = List.fold (fun s v -> s + (fst (snd (snd v))).ToString()) "" move
        (fst move.Head, (isWordVertical (fst move.Head) (fst (move.Item(1))), word))

    type state = {
        board           : boardProg
        dict            : Dict
        playerNumber    : uint32
        numberOfPlayer  : uint32
        hand            : MultiSet.MultiSet<uint32>
        boardState      : Map<coord, (char * int)>
        layedWords      : (coord * (bool * string)) list
    }

    let mkState board dict playerNumber numberOfPlayer hand bState lWords = {
        board = board
        dict = dict
        playerNumber = playerNumber
        numberOfPlayer = numberOfPlayer
        hand = hand
        boardState = bState
        layedWords = lWords
    }
    
    let state board dict playerNumber numberOfPlayer hand bState lWords = mkState board dict playerNumber numberOfPlayer hand bState lWords

module Scrabble =
    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            Print.printHand pieces st.hand
            
            let position = if st.layedWords.Length <> 0 then State.layPosition st.layedWords else ((0,0), (true, '@'))
            let coord = fst position
            let vertical = fst (snd position)
            let character = (snd (snd position)).ToString()
            
            debugPrint $"{State.getPieceData pieces}\n"
            let letters = State.handToList st.hand pieces
            let words   = State.lookupWords letters st.dict
            let sort    = List.filter (fun w -> Regex.Matches(w,$"^{character}[A-Z]+").Count > 0) words
            debugPrint $"list: {toList st.hand}\n"
            debugPrint $"chars: {letters}\n"
            //for w in words do
            //    debugPrint $"word: {w}\n"
            //debugPrint "--------------------------------------\n"
            for w in (State.sortWordsByValue words pieces) do
                debugPrint $"word: {w}\n"
            
            //let letters = ["C";"A";"T";]
            //let result = State.lookupWords letters "" st.dict
            //debugPrint $"RESULT_VARIABLE = %A{result.Length}\n"
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            if st.boardState.Count > 0 then
                //debugPrint $"word layable (0,0): {State.isWordLayable st.boardState (0,0) false}\n"
                //debugPrint $"{st.boardState.[(0,0)]}\n"
                //debugPrint $"{st.boardState.[(0,1)]}\n"
            debugPrint $"board count: {st.boardState.Count}\n"
            //debugPrint $"board layPosition: {State.layPosition st.boardState st.layedWords}\n"
            
            let cut = State.cutWord st.boardState coord vertical (State.sortWordsByValue words pieces)
            State.layWord pieces (snd cut) (fst cut) vertical cstream
            
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

        fun () -> playGame cstream tiles (State.state boardP dict playerNumber numPlayers handSet Map.empty List.empty)