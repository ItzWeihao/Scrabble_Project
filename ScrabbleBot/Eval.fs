// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add a b = failwith "Not implemented"      
    let div a b = failwith "Not implemented"      

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let arithEval a : SM<int> = failwith "Not implemented"      

    let charEval c : SM<char> = failwith "Not implemented"      

    let boolEval b : SM<bool> = failwith "Not implemented"


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

           
    let rec arithEval2 a =
        match a with
        | N n -> prog { return n }
        | V v -> prog { return! lookup v }
        | WL -> prog { return! wordLength }
        | PV pv -> prog { let! a = arithEval pv
                          return! pointValue a }
        | Add(a, b) -> prog { let! x = (arithEval2 a)
                              let! y = (arithEval2 b)
                              return (( + ) x y)}
        | Sub(a, b) -> prog { let! x = (arithEval2 a)
                              let! y = (arithEval2 b)
                              return (( - ) x y)}
        | Mul(a, b) -> prog { let! x = (arithEval2 a)
                              let! y = (arithEval2 b)
                              return (( * ) x y)}
        | Div(a, b) -> prog { let! x = (arithEval2 a)
                              let! y = (arithEval2 b)
                              if y <> 0
                              then return (( / ) x y)
                              else return! fail DivisionByZero}
        | Mod(a, b) -> prog { let! x = (arithEval2 a)
                              let! y = (arithEval2 b)
                              if y <> 0
                              then return (( % ) x y)
                              else return! fail DivisionByZero}
        | CharToInt c -> prog { let! i = charEval2 c
                                return int i }
    
    and charEval2 c =
        match c with
        | C c -> prog { return c }
        | CV a -> prog { let! a1 = arithEval2 a
                         return! characterValue a1 }
        | ToUpper c -> prog { let! c1 = charEval2 c
                              return System.Char.ToUpper c1 }
        | ToLower c -> prog { let! c1 = charEval2 c
                              return System.Char.ToLower c1 }
        | IntToChar a -> prog { let! a1 = arithEval2 a
                                return char (a1 + int '0') }
    let rec boolEval2 b =
        match b with
        | TT -> prog { return true }
        | FF -> prog { return false }
        
        | AEq(a, b) -> prog { let! a1 = arithEval2 a
                              let! b1 = arithEval2 b
                              return (( = ) a1 b1) }
        | ALt(a, b) -> prog { let! a1 = arithEval2 a
                              let! b1 = arithEval2 b
                              return (( < ) a1 b1) }
        | Not(b) -> prog { let! b1 = boolEval2 b
                           return not b1 }
        | Conj(b1, b2) -> prog { let! b3 = boolEval2 b1
                                 let! b4 = boolEval2 b2
                                 return (( && ) b3 b4) }
        
        | IsVowel(c) -> prog { let! c1 = charEval2 c
                               return "aeiouæøåAEIOUÆØÅ".Contains(c1) }
        | IsConsonant(c) -> prog { let! c1 = charEval2 c
                                   return not ("aeiouæøåAEIOUÆØÅ".Contains(c1)) }

    let rec stmntEval2 stm =
        match stm with
        | Declare s -> prog { return! declare s }
        | Ass(s, a) -> prog { let! a1 = arithEval2 a
                              return! update s a1 }
        | Skip -> prog { return () }
        | Seq(s1, s2) -> prog { do! stmntEval2 s1
                                let! s3 = stmntEval2 s2
                                return s3}
        | ITE(b, s1, s2) -> prog { let! bool = boolEval2 b
                                   do! push
                                   if bool
                                   then do! stmntEval2 s1
                                   else do! stmntEval2 s2
                                   do! pop}
        | While(b, s) -> prog { let! bool = boolEval2 b
                                do! push
                                if bool
                                then do! stmntEval2 s
                                     do! stmntEval2 (While (b, s))
                                else return ()
                                do! pop }

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"