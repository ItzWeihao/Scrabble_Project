namespace CatSquish

module Dictionary =
    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
    
    val empty : unit -> Dict
    val insertWord : string -> Dict -> Dict
    val step : char -> Dict -> (bool * Dict) option
    val containsWord : string -> (Dict -> bool)