module ICPC
open System

let applyRuleOne (input: string) = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's first rule
    let noCommas = input.Split(',')|> Array.toList // split on comma and convert the array to a list to use list functions later 
    ///// potential vars 
    let n = 0;
    let count = 0; 
    //////////////important functions////////////
    let FindWord (p: List<string>) =
      p.[p.Length-1]

    let findPos (st1: string) (st2: string) =
      st1.IndexOf(st2)
    
    let AddComm (pos: int)(words:string) =
      words.Insert(pos, ",")
    
    
    //this is the idea now figure out the iteration!!!
    let commas = 
       let word = FindWord (noCommas.[0].Split()|> Array.toList)
       let pos = findPos (noCommas.[1]) (word)
       let NewString =  noCommas.[0] + AddComm (pos + word.Length) noCommas.[1]
       NewString

    commas

   
    
   
    
    

    //failwith "Function not implemented"
    //words

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
    Console.WriteLine(applyRuleOne("hello, I hello am, the man, hello okay?"))
    0 // return an integer exit code
