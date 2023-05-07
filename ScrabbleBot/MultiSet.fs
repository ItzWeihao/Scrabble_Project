// Insert your MultiSet.fs file here. All modules must be internal
namespace CatSquish

module internal MultiSet =
    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>
    
    // Creates an empty multiset
    let empty = MS Map.empty
    
    // Given a multiset s it returns true if s is empty else false
    let isEmpty (MS s) = s.IsEmpty
    
    // Given a multiset s it returns the sum of number of all elements in s
    // 0u = returns uint32
    let size (MS s) = Map.fold (fun acc _ item -> acc + item) 0u s
    
    // Given an element a and a multiset s, it returns true if a is an element of s if not it returns false
    let contains a (MS s) = s.ContainsKey(a)
    
    // Given an element a and a multiset s, it returns the number (uint32) of occurrences a is in s  
    let numItems a (MS s) = s.TryFind a |> Option.defaultValue 0u
    
    // Given an element a, multiset s and number n, it adds a to s, n times
    let add a n (MS s) = MS (s.Add (a, (numItems a (MS s) + n)))
    
    // Given an element a and multiset s, it adds a into s 1 time
    let addSingle a (s : MultiSet<'a>) = add a 1u s
    
    // Given an element a, multiset s and number n, it returns where a has been removed from s, n times
    let remove a n (MS s) =
        let value = numItems a (MS s)     
        if not (value = 0u)  && value > n
        then MS (s.Add (a, value - n))
        else MS (s.Remove a)
    
    // Given an element a and multiset s, it removes a from s 1 time
    let removeSingle a s = remove a 1u s
    
    // Given a folding function f, accumulator acc and multiset s, it returns folded multiset s
    let fold f acc (MS s) = Map.fold f acc s
    
    // Given a folding function f, accumulator acc and multiset s, it returns foldBacked multiset s (foldBack = reverse fold)
    let foldBack f (MS s) acc = Map.foldBack f s acc
    
    // Essentially puts all element from List lst into an empty MultiSet containing exactly elements of lst
    let ofList lst = List.fold (fun acc item -> addSingle item acc) empty lst
        
    // Takes a Multiset s and converts it into a List
    let toList (s : MultiSet<'a>) = fold (fun acc a item -> acc@[for _ in 1..(int item) -> a]) [] s
    
    // Takes a mapping function f and MultiSet s, it returns mapped functions
    let map f (s : MultiSet<'a>) = fold (fun acc a item -> add (f a) item acc) empty s
    
    // Unions s1 with s2 meaning it contains the maximum number of occurrences of elements in both sets (no duplicates)
    let union (s1 : MultiSet<'a>) (s2 : MultiSet<'a>) =
        (s1, s2) ||>
        fold (fun acc a item ->
            let accItem = numItems a acc
            if item > accItem
            then add a (item - accItem) acc
            else acc)
        
    // Sums s1 and s2 meaning it contains all elements from both MultiSets (can have duplicates)
    let sum (s1 : MultiSet<'a>) (s2 : MultiSet<'a>) =
        (s1, s2) ||>
        fold (fun acc a item -> add a item acc)
    
    // Subtracts any element in s1 with s2 meaning if s1 has any element in s2 it gets removed (anything s2 has in common with s1 gets removed)
    let subtract (s1 : MultiSet<'a>) (s2 : MultiSet<'a>) =
        (s1, s2) ||>
        fold (fun acc a item -> remove a item acc)
       
    // Intersection between s1 and s2 meaning we take any element s1 has that is common in s2
    let intersection (s1 : MultiSet<'a>) (s2 : MultiSet<'a>) =
        (empty, s2) ||>
        fold (fun acc a item ->
            match numItems a s1 with
            | 0u -> acc
            | s2Item when s2Item < item -> add a s2Item acc
            | _ -> add a item acc)
