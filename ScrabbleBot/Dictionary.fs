namespace CatSquish
module Dictionary =
    open System
    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
    
    let empty () = Leaf false
    
    let rec insertWord (word: string) (dict: Dict) =
        let words = List.ofArray(word.Split())
        
        let rec insertToTree (word: string) (dict: Dict) =
            match dict with
            | Leaf _ when word.Length = 0 -> Leaf true
            | Node (_, dict) when word.Length = 0 -> Node(true, dict)  
            | Leaf isEndOfWord ->
                let newDict = Map.add word.[0] (insertWord word.[1..] (empty())) Map.empty
                Node (isEndOfWord, newDict)
            | Node (isEndOfWord, dict) ->
                match Map.tryFind word.[0] dict with
                | Some node ->
                    let newNode = insertWord word.[1..] node
                    Node(isEndOfWord, Map.add word.[0] newNode dict)
                | None ->
                    let newNode = insertWord word.[1..] (empty())
                    Node(isEndOfWord, Map.add word.[0] newNode dict)
            
        List.fold (fun acc word -> insertToTree word acc) dict words
        
    let step (c : char) (dict: Dict) =
        match dict with
        | Node (_, children) ->
            match  Map.tryFind c children with
            
            | Some child ->
                match child with
                | Leaf isEndOfWord -> Some (isEndOfWord, child)
                | Node (isEndOfWord, _) -> Some (isEndOfWord, child)
                
            | None -> None
        | Leaf _ -> None
    
    let rec containsWord (word: string) (dict: Dict) : Boolean = 
        
        let innerLookup (acc : (bool * Dict) option) (element: char) =
            
            match acc, step element dict with
            
            | Some(_, value), Some(isEndOfWord, node) -> Some (isEndOfWord, node)
            
            | _, _ -> None
            
        let result = List.fold innerLookup (Some (false, dict)) (Seq.toList word)
        match result with
        
        | Some (isEndOfWord, _) -> isEndOfWord
        
        | None -> false
    
    