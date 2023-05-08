namespace CatSquish

open CatSquish.MultiSet
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

    let getPieceValue (pieces: Map<uint32,tile>) (piece: char) =
        snd (pieces.Item((uint32 piece)-64u).MinimumElement)
        
    let isWordLayable (board: Map<coord, (char * int)>) (coord: coord) (isVertical: bool) : bool =
        let i = match isVertical with | true -> (fst coord, snd coord+1) | false -> (fst coord+1, snd coord)
        //for l in board do debugPrint $"coord: {l.Key}, char: {l.Value}\n"
        not (Map.exists (fun n _ -> debugPrint $"i: {i}, n: {n}\n"; n = i) board)
    
    let layPosition (board: Map<coord, char * int>) : coord * (bool * char) =
        match board.Count with
        | 0 -> ((0,0), (true, '#'))
        | _ -> 
    
    let layWord (pieces: Map<uint32,tile>) (word: string) (startPos: coord) (isVertical: bool) (cstream: Stream) =
        let mutable i = match isVertical with | true -> (fst startPos, -1) | false -> (-1, snd startPos) 
        RegEx.parseMove (List.fold (fun s v ->
            i <- match isVertical with | true -> (fst i, snd i+1) | false -> (fst i+1, snd i)
            s + $"{fst i} {snd i} {int v-64}{v}{getPieceValue pieces v} "
            ) "" (word.ToCharArray() |> List.ofArray)) cstream
    
    // <(piece num; (piece char; piece amount)); ...> --> <(piece num; piece char); ...>
    let getPieceData (pieces: Map<uint32,tile>) =
        Map.fold (fun s k v -> Map.add k (fst (Set.toList v).Head) s) Map.empty pieces
    
    let sortWordsByValue (words: string list) (pieces: Map<uint32,tile>) : string list =
        List.sortByDescending (fun s -> s.ToCharArray() |> List.ofArray |> List.fold (fun a v -> a + (getPieceValue pieces v)) 0) words
    
    let lookupWords (hand: string List) (dict: Dict) : string list =
        let rec rackLookup l w d =
            List.fold (fun s v ->
                    let wordn = if lookup w d then [w] else List.empty
                    s @ wordn @ (rackLookup (List.filter ((<>) v) l) (w+v) d)
                    ) List.empty l
        rackLookup (hand @ [""]) "" dict |> Seq.distinct |> List.ofSeq
    
    // <(char num; char amount); ...> --> [char num; ...] --> [char string; ...]
    let handToList (hand: MultiSet<uint32>) (pieces: Map<uint32,tile>) : string list =
        List.fold (fun s v -> s @ [(getPieceData pieces).Item(v).ToString()]) List.empty (toList hand)
        
    let isWordVertical (coord1: coord) (coord2: coord) : bool =
        (snd coord1) <> (snd coord2)
        
    let moveToList (move: (coord * (uint32 * (char * int))) list) : coord * (bool * string) =
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
            
            debugPrint $"{State.getPieceData pieces}\n"
            let letters = State.handToList st.hand pieces
            let words   = State.lookupWords letters st.dict
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
                debugPrint $"word layable (0,0): {State.isWordLayable st.boardState (0,0) false}\n"
                //debugPrint $"{st.boardState.[(0,0)]}\n"
                //debugPrint $"{st.boardState.[(0,1)]}\n"
            debugPrint $"board count: {st.boardState.Count}\n"
                
            State.layWord pieces (State.sortWordsByValue words pieces).Head (0,0) true cstream
            
            //let input = System.Console.ReadLine()
            //RegEx.parseMove input cstream
            
            //debugPrint $"Player %d{st.playerNumber} -> Server:\n%A{move}\n"
            //debugPrint $"Player %d{st.playerNumber} <- Server:\n%A{move}\n"
            
            match (recv cstream) with
            | RCM (CMPlayed (playerId, move, points)) ->
                (* Successful play by other player. Update your state *)
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let addWord = st.layedWords @ [State.moveToList move]
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard addWord
                aux st'
            | RCM (CMPlaySuccess (move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removePiece = List.fold (fun acc elm -> removeSingle (fst(snd(elm))) acc) st.hand move
                let addPiece = List.fold (fun acc elm -> add (fst elm) (snd elm) acc) removePiece newPieces
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let addWord = st.layedWords @ [State.moveToList move]
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
        let handSet = List.fold (fun acc (x, k) -> add x k acc) empty hand

        fun () -> playGame cstream tiles (State.state boardP dict playerNumber numPlayers handSet Map.empty List.empty)