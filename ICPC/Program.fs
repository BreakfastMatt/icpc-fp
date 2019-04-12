module ICPC
open System

let applyRuleOne (input: string) = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's first rule
    let noCommas = input.Split(',')|> Array.toList // split on comma and convert the array to a list to use list functions later 
    let n = 0;
    //////////////important functions////////////
    let FindWord (p: List<string>) =
      p.[p.Length-1]

    let word = FindWord (noCommas.[0].Split()|> Array.toList) //word next to the comma 

    let findPos (st1: string) (st2: string) =  // finds where in the string the word is 
      st1.IndexOf(st2)
    
    let AddComm (pos: int)(words:string) =  // adds comma in desired place 
      words.Insert(pos, ",")
  
    let commas n (xs: List<string>) =  //  applies important functions 
       let pos = findPos (xs.[n]) (word)
       let NewString =  AddComm (pos + word.Length) xs.[n]
       NewString
    
    let rec addCommas (xs: List<string>) word nString n = // applies the commas function to each element of the list but this could 
      match n < xs.Length  with                           // have been done with the List functions 
       |false -> nString
       |_ -> addCommas xs word (nString + commas n xs) (n+1)

    addCommas noCommas word "" n

   
let applyRuleTwo input = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's second rule
    let input = failwith "Function not implemented"
    input 

let commaSprinkler input =
    failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
