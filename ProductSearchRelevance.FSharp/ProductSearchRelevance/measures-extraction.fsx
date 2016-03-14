open System
open System.Text.RegularExpressions
open System.Net
open System.Collections.Concurrent

(*
Basic string operations
*)

// cleansing

let inline cleanHtml (txt:string) = WebUtility.HtmlDecode txt

let inline lowerCase (txt:string) = txt.ToLowerInvariant()

let manySpaces = Regex(@"\s+", RegexOptions.Compiled)
let inline cleanSpaces (txt:string) = manySpaces.Replace(txt, " ")

// processing numbers

// remove , separating thousands
let thousandsSeparator = Regex(@"(\d+),(?=\d{3}(\D|$))", RegexOptions.Compiled)
let inline cleanThousands (txt:string) =
    thousandsSeparator.Replace(txt,"$1")

let dots = Regex(@"([^0-9])\.|\.([^0-9]|$)", RegexOptions.Compiled)
let inline cleanDots (txt:string) =
    dots.Replace(txt,"$1 $2")

// misses A/C
let fraction = Regex(@"([^0-9])\/|\/([^0-9]|$)", RegexOptions.Compiled)
let inline cleanFractions (txt:string) =
    fraction.Replace(txt, "$1 $2")

// nuke punctuation signs, except .
// commas separating 1000s need to be handled first
let punctuation = Regex(@"[!?;:,\[\]\(\)\+\-&#_ې۪�]", RegexOptions.Compiled)
let inline cleanPunctuation (txt:string) = punctuation.Replace(txt, " ")

// careful, symbols used for feet and inches
let exclamation = Regex(@"['\""]", RegexOptions.Compiled)
let cleanExclamation (txt:string) = exclamation.Replace(txt, " ")

let dollars = Regex("([0-9])\s*\$ | \$\s*([0-9])", RegexOptions.Compiled)
let cleanDollars (txt:string) = dollars.Replace(txt, "$1 dollars $2")

let units = [
    "sq. ft.", @"(square|sq)\.?\s*('|ft\.|ft|foot|feet)"
    "cu. ft.", @"(cubic|cu)\.?\s*('|ft\.|ft|foot|feet)"
    "pct.", "%"
    "in.", @"""|inches|inch|in\.|in\s"
    "ft.", @"'|ft\.|ft|foot|feet"
    "lb.", @"pounds|pound|lbs|lb\.|lb"
    "gal.", @"gallons|gallon|gal\s|gal\."
    "oz.", @"ounces|ounce|oz\.|oz"
    "volt", @"volts|volt|V\s|v\s" // check 5-v
    "watt", @"watts|watt" // check 5 W vs. width?
    "amp", @"ampere|amps|amp\."
    "btu", @"BTU"
    "yd.", @"yards|yard|yd\.|yd"
    "mm", @"millimeters|millimeter|mm\.|mm\s"
    ]

let unitsReplacers =
    units
    |> List.map (fun (replacement,pattern) ->
        let regex = Regex(sprintf "([0-9]\s*)(%s)" pattern, RegexOptions.Compiled)
        fun (txt:string) -> regex.Replace(txt, sprintf "$1 %s " replacement))

let inline cleanUnits (txt:string) = 
    unitsReplacers 
    |> List.fold (fun result f -> f result) txt

let multiply = Regex("([0-9\.\s])(\*|x|X|by\s)(\s*[0-9])", RegexOptions.Compiled)
let inline cleanMultiply (txt:string) = multiply.Replace(txt, "$1 x $3")

let inline letterNumber (text:string) =
    Regex.Replace(text, "([a-zA-Z])([0-9])", "$1 $2")

let inline numberLetter (text:string) =
    Regex.Replace(text, "([0-9])([a-zA-Z])", "$1 $2")

let lettersNumbers = [ 
    "zero", 0
    "one", 1
    "two", 2
    "tree", 3
    "four", 4
    "five", 5
    "six", 6
    "seven", 7
    "eight", 8
    "nine", 9 ]

let inline replaceNumbers (txt:string) =
    lettersNumbers 
    |> Seq.fold (fun (t:string) (number,value) -> 
        t.Replace(sprintf " %s " number, sprintf " %i " value)) txt
        
let trim (txt:string) = txt.Trim ()

let preprocess =
    cleanHtml 
    >> lowerCase
    >> cleanThousands 
    >> cleanDots
    >> cleanFractions
    >> cleanPunctuation
    >> letterNumber
    >> numberLetter
    >> replaceNumbers
    >> cleanDollars
    >> cleanUnits
    >> cleanMultiply
    >> cleanExclamation
    >> cleanSpaces
    >> trim

#load "Dependencies.fsx"
open FSharp.Data

[<Literal>]
let trainPath = @"../data/train.csv"
type Train = CsvProvider<trainPath,Schema=",,,,float">

let test = 
    Train.GetSample().Rows
    |> Seq.map (fun x -> x.Product_title, x.Product_title |> preprocess)
    |> Seq.take 100
    |> Seq.iter (fun (a,b) -> 
        printfn "%s" a
        printfn "  %s" b)

Train.GetSample().Rows
|> Seq.map (fun x -> x.Search_term)
|> Seq.filter (fun x -> x.Contains " by ")
