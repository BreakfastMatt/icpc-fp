module ICPC
open System

let applyRuleOne (input: string) n = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's first rule
    let noCommas = input.Split ',' |> Array.toList // split on comma and convert the array to a list to use list functions later 
    let n = 0;
    //////////////important functions////////////
    let FindWord (p: List<string>) =
      p.[p.Length-1]

    let word = FindWord (noCommas.[n].Split()|> Array.toList) //word next to the comma 

    let findPos (st1: string) (st2: string) =  // finds where in the string the word is 
      st1.IndexOf(st2)
    
    let AddComm (pos: int)(words:string) =  // adds comma in desired place 
      words.Insert(pos, ",")
    
    let n = 0
    let commas n (xs: List<string>) =  //  applies important functions 
       let pos = findPos (xs.[n]) (word)
       let NewString =
           match xs.[n].Length <> (pos + word.Length) with 
           |true -> AddComm (pos + word.Length) xs.[n]
           |_-> xs.[n]
       
       NewString
    
    let rec addCommas (xs: List<string>) word nString n = // applies the commas function to each element of the list but this could 
      match n < xs.Length  with                           // have been done with the List functions 
       |false -> nString
       |_ -> addCommas xs word (nString + commas n xs) (n+1)

    addCommas noCommas word "" n

let applyRuleTwo (input: string) = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's second rule
    let noCommas = input.Split ',' |> Array.toList // split on comma and convert the array to a list to use list functions later 
    let n = noCommas.Length;
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
       let NewString =
          match pos <> 0 with 
          |true -> AddComm pos xs.[n]
          |_-> xs.[n]
       
       NewString
    
    let rec addCommas (xs: List<string>) word nString n = // applies the commas function to each element of the list but this could 
      match n < xs.Length  with                           // have been done with the List functions 
       |false -> nString
       |_ -> addCommas xs word (commas n xs + nString ) (n-1)

    addCommas noCommas word "" n

let commaSprinkler (input: string) =    
    let n = 0   
    let rec ruleThree i string = 
       let newString = applyRuleTwo (applyRuleOne string i)
       let old = string
       match old = newString with 
       |true -> newString 
       |_ -> match i+1 < ((input.Split(',')|> Array.toList).Length- 1) with  
             |true -> ruleThree (i+1) newString 
             |_ -> ruleThree 0 newString
    match input = "" || input.[0] = ' ' ||  input.[0] = '.' || input.[input.Length - 1] = ' ' || input.[input.Length - 1] = ' '  with 
    |true -> None 
    |_ -> Some (ruleThree n input)
       

let determineLines (input:string) lineWidth =  
//can call this function in the rivers function below to determine the different lines (based on the line width)
    let rec loop count lines startIndex= 
        match (count = lineWidth) with
        |true -> lines //return lines (we will use this in the rivers function below)
        |_  ->  
        let value = input.Substring(startIndex,lineWidth) // need to have some way to check if this will chop off a word ....
        let lines = lines@[value] //double check that this is adding to end of line list
        loop (count+1) lines  (startIndex+lineWidth) //need to check if startIndex + lineWidth goes over length of original input (simple check)
    failwith "Not implemented"

let rivers input =
//Have recursive function that will adjust the line width and perform the necessary river functionality (return the line width and the length of the river)
//Have a variable that will keep track of the max river & the width that created that river 
//(only update/change this in subsequent recursive calls if you find a line width that made a larger river)
    let rec riverStuff lineWidth maxRiver = //might need to add more inputs to this (idk yet)
        let lines = determineLines input lineWidth
        failwith "Not implemented"
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
