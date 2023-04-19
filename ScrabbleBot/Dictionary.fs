namespace CatSquish
module Dictionary =
    open System
    open System.Collections.Generic
    type Dict =
        | Leaf of bool
        | Node of bool * Dictionary<char, Dict>
    
    let empty () = Leaf false
    
    let foundation = ">"
    
    let rec insert (s : string) (d : Dict) =
        let splitWord = List.ofArray(s.Split())
        
        let rec insertToTree (s : string) =
            function
            | Leaf _ when s.Length = 0 -> Leaf true
            
            | Node (_, dict) when s.Length = 0 -> Node(true, dict)
            
            | Leaf b ->
                let dict = Dictionary ()
                dict.[s.[0]] <- insertToTree s.[1..] (empty ())
                Node (b, dict)
                
            | Node (b, dict) ->
                match dict.TryGetValue s.[0] with
                | true, d ->
                    dict.[s.[0]] <- insertToTree s.[1..] d
                    Node(b, dict)
                | false, _ ->
                    dict.[s.[0]] <- insertToTree s.[1..] (empty ())
                    Node(b, dict)
            
        let words = splitWord
        List.fold (fun acc word -> insertToTree word acc) d words
    
    let step (c : char) =
        function
        | Node (_, dict) ->
            match dict.TryGetValue c with
            | true, d ->
                match d with
                | Leaf b -> Some (b, d)
                | Node (b, _) -> Some (b, d)
            | false, _ -> None
        | Leaf _ -> None
    
    let reverse = step foundation.[0]
    
    let rec lookup (s: string) (dict: Dict) : Boolean =
        let innerLookup acc element =
            match acc with
            | Some(_, value) -> step element value
            | None -> None
        let b = List.fold innerLookup (Some (false, dict)) (Seq.toList s)
        match b with
        | Some (b, _) -> b
        | None -> false
    
    