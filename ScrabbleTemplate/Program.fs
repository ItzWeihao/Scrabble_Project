// Learn more about F# at http://fsharp.org

open ScrabbleUtil

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

[<EntryPoint>]
let main argv =
    DebugPrint.toggleDebugPrint true // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.White
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()

    // let board   = ScrabbleUtil.StandardBoard.standardBoard ()
    let board      = InfiniteBoard.infiniteBoard ()
    // let board   = ScrabbleUtil.RandomBoard.randomBoard ()
    // let board   = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
    // let board   = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
    // let board   = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)
    // let board   = ScrabbleUtil.HoleBoard.holeBoard ()
    // let board   = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()

    let words      = readLines "../../../Dictionaries/English.txt"
    let handSize   = 7u
    let timeout    = None
    let tiles      = English.tiles 1u
    let seed       = None
    let port       = 13001
    let dictAPI    = Some (CatSquish.Dictionary.empty, CatSquish.Dictionary.insertWord, CatSquish.Dictionary.step, None) // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
    
    // Uncomment this line to call your client
    let (dictionary, time) = time (fun () -> Dictionary.mkDict words dictAPI)
    
    // Uncomment to test your dictionary
    DebugPrint.debugPrint ("Dictionary test successful\n")
    let incorrectWords = Dictionary.test words 10 (dictionary false) // change the boolean to true if using a GADDAG
    
    match incorrectWords with
    | [] -> DebugPrint.debugPrint ("Dictionary test successful!\n")
    | _ -> DebugPrint.debugPrint ("Dictionary test failed for at least the following words: \n")
           List.iter (fun str -> DebugPrint.debugPrint $"%s{str}\n") incorrectWords
    
    let players = spawnMultiples "CatSquish" dictionary CatSquish.Scrabble.startGame 1
    //let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 2

    do ScrabbleServer.Comm.startGame
           board dictionary handSize timeout tiles seed port players
    
    DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore

    0