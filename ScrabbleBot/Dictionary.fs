namespace CatSquish
module Dictionary =
    open System
    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
    
    let empty () = Leaf false
    
    let foundation = ">"
    
    let rec insert (s : string) dict =
        let splitWord = List.ofArray(s.Split())
        
        let rec insertToTree (s : string) dict =
            match dict with
            | Leaf _ when s.Length = 0 -> Leaf true
            
            | Node (_, dict) when s.Length = 0 -> Node(true, dict)
            
            | Leaf b ->
                let dict = Map.add s.[0] (insert s.[1..] (empty())) Map.empty
                Node (b, dict)
                
            | Node (b, dict) ->
                match Map.tryFind s.[0] dict with
                | Some node ->
                    let newNode = insert s.[1..] node
                    Node(b, Map.add s.[0] newNode dict)
                | None ->
                    let newNode = insert s.[1..] (empty())
                    Node(b, Map.add s.[0] newNode dict)
            
        let words = splitWord
        List.fold (fun acc word -> insertToTree word acc) dict words
    
    let step (c : char) =
        function
        | Node (_, dict) ->
            match  Map.tryFind c dict with
            | Some node ->
                match node with
                | Leaf b -> Some (b, node)
                | Node (b, _) -> Some (b, node)
            | None -> None
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
    
    