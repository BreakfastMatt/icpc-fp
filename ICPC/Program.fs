module ICPC
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


let multiDots (input: string ) = //used for input validation in CommaSprinkler 
   let n = 0 
   let rec checkDots (sent:string) n cnt=
      match sent.[n] = '.' with
      |true -> match cnt+1 > 1 with 
               |true -> cnt+1
               |_ -> match n+1 < sent.Length  with 
                     |true -> checkDots sent (n+1) cnt+1
                     |false -> cnt
      |_ -> 
       let cnt = 0
       checkDots sent (n+1) cnt

   checkDots input n 0

let multiSpace (input: string ) = //used for input validation in CommaSprinkler 
   let n = 0 
   let rec checkDots (sent:string) n cnt=
      match sent.[n] = ' ' with
      |true -> match cnt+1 > 1 with 
               |true -> cnt+1
               |_ -> match n+1 < sent.Length  with 
                     |true -> checkDots sent (n+1) cnt+1
                     |false -> cnt
      |_ -> 
       let cnt = 0
       checkDots sent (n+1) cnt

   checkDots input n 0

let containsUpper (input: string) =
   let rec itter n b =
      match n < input.Length with
      |false -> b
      |_ -> match input.[n] with 
            |'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z' -> let b = true 
                                                                                                                        b
            |'?'|'>'|'<'|';'|'_' -> let b = true 
                                    b
            |_ -> itter (n+1) b
   itter 0 false

let startsWithWord (input: string)=
    match input.[0] with
    |'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'-> true
    |_ -> false 

let applyRules (input: string) =
    let n = 0  
    let rec ruleThree i s = 
       let newString = applyRuleTwo (applyRuleOne s i) 
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
         |_ -> match startsWithWord input with 
               |false -> None
               |_ -> match multiDots input > 1 with
                     |true -> None
                     |_ -> match containsUpper input with 
                           |true -> None
                           |_-> match input.[input.Length - 1] with 
                                |'.'-> Some (ruleThree n input)
                                |_-> None  
                        
                     
                     
let commaSprinkler (input: string) =    
    let numOfSplits = (input.Split(',')|> Array.toList).Length
    
    match numOfSplits = 0 with
    |true -> None
    |false -> let output = applyRules input
              output           

let addSpaces (line:string) (lineWidth:int) =
//will populate the rest of the line with spaces (if a word is chopped off or it is the end of the line and the line is too short)
    let lengthRemaining = lineWidth - line.Length
    let rec addThemSpaces count (line:string) = 
        match count=lengthRemaining with
        |true -> line
        |_ -> addThemSpaces (count+1) (line+" ")
    addThemSpaces 0 line

let checkIfWordChoppedOff (potentialLine:string) (lineWidth:int) (lastChar:char) = 
    let listOfChars = (potentialLine.ToCharArray () |> Array.toList)
    let len = listOfChars.Length
    match lastChar = ' ' || listOfChars.[len] = '.' with  //check what it's allowed to end in...
    |true -> potentialLine //ends in valid char (and word not chopped off)
    |_ -> //chopped off word
    let rec removeChoppedWordChars (listOfChars:char list) = 
        let Len = listOfChars.Length 
        match (listOfChars.[Len] = ' ') with
        |true -> listOfChars.ToString ()
        |_ -> failwith "Not finished yet"
        //remove last char in list somehow...
        //removeChoppedWordChars listOfChars (Len-1)
    removeChoppedWordChars listOfChars
    
    
let chopUpIntoLines (input:string) (lineWidth:int) =  //this would replace the determineLines function below (and make mapWordIndices function obsolete)
    let rec breakIntoLines (lines:string list) (startIndex) =
        match ((startIndex + lineWidth) < input.Length) with
        |false -> addSpaces (input.Substring(startIndex)) lineWidth  //should hopefully run this if it is the last line... (check)
        |_ -> 
        let potentialLine = input.Substring(startIndex,lineWidth)
        let charList = input.Substring(startIndex,lineWidth+1).ToCharArray () |> Array.toList
        let lastChar = charList.[charList.Length]
        failwith "Stop"
        
    failwith "Not finished yet"

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
       // let lines = determineLines input lineWidth
        riverStuff 5 5
    match input with 
    |"" -> None 
    |_-> match (input.Split(',')|> Array.toList).Length < 2 with 
         |true -> None
         |_ -> match startsWithWord input with 
               |true -> None
               |_ -> match multiDots input > 1 with
                     |true -> None
                     |_ -> match containsUpper input with 
                           |true -> None
                           |_-> match input.[input.Length - 1] with 
                                |'.'-> None
                                |_-> None  
        //failwith "Not implemented"
    //failwith "Not implemented"

[<EntryPoint>]
let main argv =
    0 // return an integer exit code
