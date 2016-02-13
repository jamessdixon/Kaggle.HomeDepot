
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

let brandNames =
    CsvData.attributes.Rows
    |> Seq.where (fun r -> r.Name = "MFG Brand Name")
    |> Seq.map (fun r -> r.Product_uid, r.Value.ToLowerInvariant())
    |> Seq.cache

let nonBrandAttributes =
    CsvData.attributes.Rows
    |> Seq.where (fun r -> r.Name <> "MFG Brand Name")
    |> Seq.map (fun r -> r.Product_uid, r.Value.ToLowerInvariant())
    |> Seq.cache

let brandName productId = 
    brandNames 
    |> Seq.tryFind(fun (id,name) -> id = productId)
    |> Option.map(fun (id,name) -> name)

let nonBrandName productId = 
    nonBrandAttributes 
    |> Seq.filter(fun (id,name) -> id = productId)
    |> Seq.map(fun (id,name) -> name)
    |> Seq.reduce(fun a n -> a + " " + n )

let stemDictionary = ConcurrentDictionary<string,string>(StringComparer.OrdinalIgnoreCase)
let stem word = stemDictionary.GetOrAdd(word, (fun s -> (new EnglishStemmer()).Stem s))
let splitOnSpaces (str:string) = str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
let stemWords = splitOnSpaces >> Array.map stem >> String.concat " "

let containedIn (input:string) word =
    input.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0

let isMatch input word =
    let word' = word |> sanitizeString |> stem |> Regex.Escape
    Regex.IsMatch(input |> sanitizeString |> stemWords, sprintf @"\b%s" word', RegexOptions.IgnoreCase)

let wordCountMatch uniqueTerms matches =
    uniqueTerms
    |> Seq.filter (fun w -> matches |> Seq.contains w)
    |> Seq.length

let createFeatures (uniqueTerms:string[]) (searchTerm:string) (title:string) (description:string) 
                   (wordMatchCount:int) (titleMatches:string[]) (descriptionMatches:string[]) 
                   (attributeMatches:string[]) (brandNameMatchCount:int) = 
    [| float uniqueTerms.Length
       float searchTerm.Length
       float title.Length
       float description.Length
       float wordMatchCount
       float titleMatches.Length
       float titleMatches.Length / float uniqueTerms.Length
       float descriptionMatches.Length / float uniqueTerms.Length
       float descriptionMatches.Length
       float attributeMatches.Length
       float brandNameMatchCount |]

let features (sample:CsvData.Sample) =
    let searchTerm = sample.Query.ToLowerInvariant()
    let title = sample.Title.ToLowerInvariant()
    let description = sample.Description.ToLowerInvariant()
    let brandName = brandName sample.ProductId
    let nonBrandName = nonBrandName sample.ProductId

    let uniqueTerms = searchTerm |> splitOnSpaces |> Array.distinct
    let titleMatches = uniqueTerms |> Array.filter (isMatch title)
    let descriptionMatches = uniqueTerms |> Array.filter (isMatch description)
    let attributeMatches = uniqueTerms |> Array.filter (isMatch nonBrandName)
    let matches = Seq.concat [titleMatches; descriptionMatches; attributeMatches]
    let wordMatchCount = wordCountMatch uniqueTerms matches
    let brandNameMatchCount = 0
    createFeatures uniqueTerms searchTerm title description wordMatchCount titleMatches descriptionMatches attributeMatches brandNameMatchCount

printfn "Extracting training features..."
let inputs = 
    CsvData.getTrainSamples() 
    |> PSeq.map(fun s -> features s)
    |> Seq.toArray

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



