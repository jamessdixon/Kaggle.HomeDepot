
#I @"../packages/"

#r "Accord/lib/net45/Accord.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Accord.Math/lib/net45/Accord.Math.dll"
#r "Accord.Statistics/lib/net45/Accord.Statistics.dll"
#r "FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "StemmersNet/lib/net20/StemmersNet.dll"
#r "alglibnet2/lib/alglibnet2.dll"
#r "FuzzyString/lib/FuzzyString.dll"
#r "AForge.Neuro/lib/AForge.Neuro.dll"
#r "Accord.Neuro/lib/net40/Accord.Neuro.dll"

#load "CsvData.fs"
#load "StringUtils.fs"
#load "Core.fs"
#load "TFIDF.fs"

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open FSharp.Data
open Accord.Statistics.Models.Regression.Linear
open Iveonik.Stemmers
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent
open HomeDepot.Core
open AForge.Neuro
open Accord.Neuro
open Accord.Neuro.Learning
open Accord.Statistics
open Accord

let sanitizeCharacter characterToClean (stringBuilder:StringBuilder) =
    match characterToClean with
    | c when Char.IsLetterOrDigit c -> stringBuilder.Append c |> ignore
    | c when Char.IsWhiteSpace c -> stringBuilder.Append c |> ignore
    | '-' -> stringBuilder.Append " " |> ignore
    | '/' -> stringBuilder.Append " " |> ignore
    | _ -> ()

let sanitizeString (stringToClean:string) =
  let stringBuilder = StringBuilder()
  stringToClean.ToCharArray() 
  |> Array.iter(fun c -> sanitizeCharacter c stringBuilder)
  stringBuilder.ToString().Trim()

let brands =
    CsvData.attributes.Rows
    |> Seq.where (fun r -> r.Name = "MFG Brand Name")
    |> Seq.map (fun r -> r.Value.ToLowerInvariant())
    |> Set.ofSeq

let stemDictionary = ConcurrentDictionary<string,string>(StringComparer.OrdinalIgnoreCase)
let stem word = stemDictionary.GetOrAdd(word, (fun s -> (new EnglishStemmer()).Stem s))
let splitOnSpaces (str:string) = str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
let stemWords = splitOnSpaces >> Array.map stem >> String.concat " "

let containedIn (input:string) word =
    input.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0

let isMatch input word =
    let word' = word |> sanitizeString |> stem |> Regex.Escape
    Regex.IsMatch(input |> sanitizeString |> stemWords, sprintf @"\b%s" word', RegexOptions.IgnoreCase)

let features attrSelector productBrand (sample:CsvData.Sample) =
    let words = sample.Query
    let title = sample.Title
    let desc = sample.Description
    let uniqueWords = words |> splitOnSpaces |> Array.distinct
    let titleMatches = uniqueWords |> Array.filter (isMatch title)
    let descMatches = uniqueWords |> Array.filter (isMatch desc)
    let attrMatches = uniqueWords |> Array.filter (isMatch (attrSelector sample.ProductId))
    let wordMatchCount =
        uniqueWords
        |> Seq.filter (fun w -> Seq.concat [titleMatches; descMatches; attrMatches] |> Seq.contains w)
        |> Seq.length
    let brandNameMatch =
        match productBrand with // does query contain product brand?
        | Some bn -> if uniqueWords |> Array.exists (containedIn bn) then 1 else 0
        | None ->
          // does query contain any brand name?
          let searchedBrand = brands |> Set.filter (containedIn words) |> Seq.tryHead
          match searchedBrand with // is query brand name in product title?
          | Some b -> if b |> containedIn title then 1 else -1
          | None -> 0
    [| float uniqueWords.Length
       float words.Length
       float title.Length
       float desc.Length
       float wordMatchCount
       float titleMatches.Length
       float titleMatches.Length / float uniqueWords.Length
       float descMatches.Length / float uniqueWords.Length
       float descMatches.Length
       float attrMatches.Length
       float brandNameMatch |]

let getAttr attribMap productId =
    match attribMap |> Map.tryFind productId with
    | Some a ->
      let getAttrStr name (value:string) =
          match value.ToLowerInvariant() with
          | "yes" -> name // if true attrib, include attrib name
          | "no"  -> String.Empty
          | _     -> value
      a |> Seq.map (fun (_, name, value) -> getAttrStr name value) |> String.concat " "
    | None -> String.Empty

let getFeatures attribs attribMap sample =
    sample |> features attribs (brandName attribMap sample.ProductId)

let extractFeatures featureExtractor = 
    PSeq.ordered
    >> PSeq.map featureExtractor
    >> PSeq.toArray

let samples, trainOutput = Array.unzip examples

printfn "Extracting training features..."
let attribs = getAttr attribMap
let getFeatures' = getFeatures attribs attribMap
let trainInput = samples |> extractFeatures getFeatures'


let inputs = [|
     [| 0.0; 1.0; 1.0; 0.0 |]; // 0.0
     [| 0.0; 1.0; 0.0; 0.0 |]; // 0.0
     [| 0.0; 0.0; 1.0; 0.0 |]; // 0.0
     [| 0.0; 1.0; 1.0; 0.0 |]; // 0.0
     [| 0.0; 1.0; 0.0; 0.0 |]; // 0.0
     [| 1.0; 0.0; 0.0; 0.0 |]; // 1.0
     [| 1.0; 0.0; 0.0; 0.0 |]; // 1.0
     [| 1.0; 0.0; 0.0; 1.0 |]; // 1.0
     [| 0.0; 0.0; 0.0; 1.0 |]; // 1.0
     [| 0.0; 0.0; 0.0; 1.0 |]; // 1.0
     [| 1.0; 1.0; 1.0; 1.0 |]; // 2
     [| 1.0; 0.0; 1.0; 1.0 |]; // 2
     [| 1.0; 1.0; 0.0; 1.0 |]; // 2
     [| 0.0; 1.0; 1.0; 1.0 |]; // 2
     [| 1.0; 1.0; 1.0; 1.0 |]; // 2
     |]

let classes = [|
    0; 0; 0; 0; 0;
    1; 1; 1; 1; 1;
    2; 2; 2; 2; 2;
    |]

let outputs = Accord.Statistics.Tools.Expand(classes, -1.0, 1.0)
let activationFunction = new BipolarSigmoidFunction()
let activationNetwork = ActivationNetwork(activationFunction, 4, 5, 3) 
let randomizer = new NguyenWidrow(activationNetwork)
randomizer.Randomize() 
let teacher = new ParallelResilientBackpropagationLearning(activationNetwork)

let mutable error = 1.0
while error > 1e-5 do
    error <- teacher.RunEpoch(inputs, outputs)
    ()



