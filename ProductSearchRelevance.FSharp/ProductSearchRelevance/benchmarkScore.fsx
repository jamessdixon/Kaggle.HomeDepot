#r "../packages/Accord/lib/net40/Accord.dll"
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../packages/Accord.Math/lib/net40/Accord.Math.dll"
#r "../packages/Accord.Statistics/lib/net40/Accord.Statistics.dll"
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "../packages/StemmersNet/lib/net20/StemmersNet.dll"

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Collections.ParallelSeq
open Accord.Statistics.Models.Regression.Linear
open Iveonik.Stemmers

[<Literal>]
let trainDataPath = "../data/train.csv"
type Train = CsvProvider<trainDataPath>
let train = Train.GetSample()

[<Literal>]
let testDataPath = "../data/test.csv"
type Test = CsvProvider<testDataPath>
let test = Test.GetSample()

[<Literal>]
let productDescriptionsPath = "../data/product_descriptions.csv"
type Products = CsvProvider<productDescriptionsPath>
let products = Products.GetSample()

let productDescMap =
    products.Rows
    |> Seq.map (fun pd -> pd.Product_uid, pd.Product_description)
    |> Map.ofSeq
let inline productDescription uid = productDescMap |> Map.find uid

let stem word =
    let stemmer = EnglishStemmer() // NOTE stemmer not thread-safe
    let stemmed = stemmer.Stem word
    if stemmed.Length < word.Length then stemmed // HACK workaround stemmer output like "vanity" -> "vaniti"
    else word

let wordMatch (words:string) (title:string) (desc:string) =
    let isMatch input word =
        let word' = stem word |> Regex.Escape // TODO strip punctuation?
        Regex.IsMatch(input, sprintf @"\b%s" word', RegexOptions.IgnoreCase)
    let uniqueWords = words.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.distinct
    let numberInTitle = uniqueWords |> Array.filter (isMatch title) |> Array.length
    let numberInDescription = uniqueWords |> Array.filter (isMatch desc) |> Array.length
    numberInTitle, numberInDescription, uniqueWords.Length 

let inline toFloatArray (a,b,c) = [| float a; float b; float c |]

let trainInput = 
    train.Rows
    |> PSeq.map (fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid) |> toFloatArray)
    |> PSeq.toArray

let trainOutput = 
    train.Rows
    |> Seq.map (fun t -> float t.Relevance)
    |> Seq.toArray

let testInput = 
    test.Rows 
    |> PSeq.map (fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid) |> toFloatArray)
    |> PSeq.toArray

let target = MultipleLinearRegression(3, true)
let error = target.Regress(trainInput, trainOutput)
let testOutput = target.Compute(testInput)

let submission = 
    Seq.zip test.Rows testOutput
    |> Seq.map (fun (r,o) -> sprintf "%A,%A" r.Id o)
    |> Seq.toList
let outputPath = __SOURCE_DIRECTORY__ + @"../../data/benchmark_submission_FSharp.csv"
File.WriteAllLines(outputPath, "id,relevance" :: submission)
