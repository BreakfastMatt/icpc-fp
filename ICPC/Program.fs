module ICPC
open System

let applyRuleOne (input: string) n = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's first rule
    let noCommas = input.Split ',' |> Array.toList // split on comma and convert the array to a list to use list functions later 


    //////////////important functions////////////
    let FindWord (p: List<string>) = //finds last word in list
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


    let noCommas = input.Split(',')|> Array.toList // split on comma and convert the array to a list to use list functions later 
    
    //////////////important functions////////////
    let FindWord (p: List<string>) =
      p.[p.Length-1]
 
    let word = FindWord (noCommas.[0].Split()|> Array.toList) //word next to the comma 

    let findPos (st1: string) (st2: string) =  // finds where in the string the word is 
      st1.IndexOf(st2)
    
    let AddComm (pos: int)(words:string) =  // adds comma in desired place 
      words.Insert(pos, ",")

    let n = noCommas.Length;

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


let dotsInRow (input: string ) = //used for input validation in CommaSprinkler 
   let n = 0 
   let count = 0;
   let rec checkDots (sent:string) n cnt=
      match sent.[n] = '.' with
      |true -> match cnt+1 > 1 with 
               |true -> cnt+1
               |_ -> checkDots sent (n+1) cnt+1
      |_ -> 
       let cnt = 0
       checkDots sent (n+1) cnt

   checkDots input n 0
 

let commaSprinkler (input: string) =    

    let n = 0   
    let rec ruleThree i s = 
       let newString = applyRuleTwo (applyRuleOne s i) //(input.Split(',')|> Array.toList).Length
       let old = s
       match old = newString with 
       |true -> newString 
       |_ -> match i+1 < ((input.Split(',')|> Array.toList).Length- 1) with  
             |true -> ruleThree (i+1) newString 
             |_ -> ruleThree 0 newString


  
    match input with 
    |"" -> None 
    |_-> match (input.Split(',')|> Array.toList).Length < 2 with 
         |true -> None
         |_ -> match input.[0] with 
               |' ' |'.'|',' -> None
               |_ -> match input.[input.Length - 1] with 
                     |'.'-> Some (ruleThree n input)
                     |_-> None
 
let mapWordIndices (input:string) = //This will be used to prevent words from being chopped off in the determineLines function
//Returns an integer list that has the word count of each word (i.e. in "The dog said hi"  it would give an indexList of: [4,
    let listOfWords = input.Split ' ' |> Array.toList
    let maxI = listOfWords.Length
    let Length = input.Length
    let rec countDemWords (indexList:int list)  counter i = 
        match i = maxI with
        |true -> 
        let counter = (counter + listOfWords.[i].Length + 0) //+0 is there to show that it doesn't end in a space
        countDemWords (indexList@[counter]) counter (i+1)
        |_ -> 
            match counter = Length with
            |true -> indexList
            |_ -> 
            let counter = (counter + listOfWords.[i].Length + 1) //+1 is there to account for spaces
            countDemWords (indexList@[counter]) counter (i+1)
    countDemWords [] 0 0

let determineLines (input:string) lineWidth =  
//can call this function in the rivers function below to determine the different lines (based on the line width)
    let rec loop count lines startIndex= 
        match (count = lineWidth) with
        |true -> lines //return lines (we will use this in the rivers function below)
        |_  ->  
        let value = input.Substring(startIndex,lineWidth) // need to have some way to check if this will chop off a word ....
        let lines = lines@[value] //double check that this is adding to end of line list
        loop (count+1) lines  (startIndex+lineWidth) //need to check if startIndex + lineWidth goes over length of original input (simple check)
    loop 0 [] 0

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
