﻿module ICPC
open System

let applyRuleOne (input: string) n = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's first rule
    let noCommas = input.Split ',' |> Array.toList // split on comma and convert the array to a list to use list functions later 


    //////////////important functions////////////
    let FindWord (p: List<string>) = //finds last word in list
      p.[p.Length-1]

    let word = FindWord (noCommas.[n].Split(' ')|> Array.toList) //word next to the comma 

    let findPos (st1: string) (st2: string) =  // finds where in the string the word is 
      st1.IndexOf(st2)
    
    let AddComm (pos: int)(words:string) =  // adds comma in desired place 
      words.Insert(pos, ",")
    
 
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

    addCommas noCommas word "" 0

let applyRuleTwo (input: string) n = //Used in the commaSprinkler function below
//applies Dr.Sprinkler's second rule


    let noCommas = input.Split(',')|> Array.toList // split on comma and convert the array to a list to use list functions later 
    
    
    //////////////important functions////////////
    let FindWord (p: List<string>) =
      p.[p.Length-1]
 
    let word = FindWord (noCommas.[n].Split()|> Array.toList) //word next to the comma 

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
      match n > 0  with                           // have been done with the List functions 
       |false -> nString
       |_ -> addCommas xs word (commas n xs + nString ) (n-1)

    addCommas noCommas word "" (noCommas.Length - 1)

let containsUpper (input: string) =
   let rec itter n b =
      match n < input.Length - 1 with
      |false -> b
      |_ -> match input.[n] with 
            |'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z' -> true 
                                                                                                                        
            |'?'|'>'|'<'|';'|'_' -> true 
                                    
            |_ -> itter (n+1) b
   itter 0 false

let startsWithWord (input: char) =
    match input with
    |'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'-> true
    |_ -> false 

let endsOther (input: string) = 
   match input.[input.Length - 1 ] with 
   |'.' -> false 
   |_ -> true 

let applyRules (input: string) =
    let n = 0  
    let rec ruleThree i s = 
       let newString = applyRuleTwo (applyRuleOne s i) i
       let old = s
       match old = newString with 
       |true -> newString 
       |_ -> match i+1 < ((input.Split(',')|> Array.toList).Length- 1) with  
             |true -> ruleThree (i+1) newString 
             |_ -> ruleThree 0 newString 


    match input with 
    |"" -> None 
    |_-> match input.Length < 2 with 
         |true -> None
         |_ -> match startsWithWord input.[0] with 
               |false -> None
               |_ -> match  input.Contains("..") with
                     |true -> None
                     |_ -> match  input.Contains(" ") with 
                           |true -> None 
                           |false -> match containsUpper input with 
                                     |true -> None
                                     |_-> match endsOther input with 
                                          |true -> None
                                          |_-> Some (ruleThree n input)
                                         
let commaSprinkler (input: string) =    
     let output = applyRules input
     output
              
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
        riverStuff 5 5
    match input with 
    |"" -> None 
    |_-> match input.Length < 2 with 
         |true -> None
         |_ -> match startsWithWord input.[0] with 
               |true -> None
               |_ -> match  input.Contains("..") with
                     |true -> None
                     |_ -> match containsUpper input with 
                           |true -> None
                           |_-> None 
        //failwith "Not implemented"
    //failwith "Not implemented"

[<EntryPoint>]
let main argv =
    Console.WriteLine(applyRuleOne ("this is multiple..."))
    Console.WriteLine(applyRuleTwo ("this is multiple..."))
    Console.WriteLine(applyRules ("this is multiple..."))
    Console.WriteLine(containsUpper ("this is multiple..."))
    Console.WriteLine(endsOther ("this is multiple..."))
   
    0 // return an integer exit code
