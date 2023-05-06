namespace CatSquish

open System
open CatSquish.MultiSet
open ScrabbleUtil
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
    let printHand pieces hand =
        hand |> fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

// Make sure to keep your state localised in this module. It makes your life a whole lot easier.
// Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
// but it could, potentially, keep track of other useful
// information, such as number of players, player turn, etc.
module State = 
    type state = {
        board           : boardProg
        dict            : Dictionary.Dict
        playerNumber    : uint32
        numberOfPlayer  : uint32
        hand            : MultiSet.MultiSet<uint32>
        boardState      : Map<coord, (char * int)>
    }

    let mkState board dict playerNumber numberOfPlayer hand bState = {
        board = board
        dict = dict
        playerNumber = playerNumber
        numberOfPlayer = numberOfPlayer
        hand = hand
        boardState = bState
    }
    
    let state board dict playerNumber numberOfPlayer hand bState = mkState board dict playerNumber numberOfPlayer hand bState

module Scrabble =
    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            Print.printHand pieces (st.hand)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  Console.ReadLine()
            RegEx.parseMove input cstream
            
            //debugPrint $"Player %d{st.playerNumber} -> Server:\n%A{move}\n"
            
            let msg = recv cstream
            //debugPrint $"Player %d{st.playerNumber} <- Server:\n%A{move}\n"
            
            match msg with
            | RCM (CMPlayed (playerId, move, points)) ->
                (* Successful play by other player. Update your state *)
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard
                aux st'
            | RCM (CMPlaySuccess (move, points, newTiles)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removePiece = List.fold (fun acc elm -> removeSingle (fst(snd(elm))) acc) st.hand move
                let addPiece = List.fold (fun acc elm -> add (fst elm) (snd elm) acc) removePiece newTiles
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer addPiece newBoard
                aux st'
            | RCM (CMPlayFailed (playerId, move)) ->
                (* Failed play. Update your state *)
                let newBoard = List.fold (fun acc (coord, (_, (char, points))) -> Map.add coord (char, points) acc) st.boardState move
                let st' = State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand newBoard
                aux st'
            | RCM (CMPassed playerId) ->
                debugPrint $"Player {playerId} died!\n"
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer st.hand st.boardState)
            //| RCM (CMForfeit playerId) ->
            //| RCM (CMChange (playerId, numberOfTiles)) ->
            | RCM (CMChangeSuccess newTiles) ->
                let addPiece = List.fold (fun acc elm -> add (fst elm) (snd elm) acc) st.hand newTiles
                aux (State.mkState st.board st.dict st.playerNumber st.numberOfPlayer addPiece st.boardState)
            //| RCM (CMTimeout playerId) ->
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
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

        fun () -> playGame cstream tiles (State.state boardP dict playerNumber numPlayers handSet Map.empty)