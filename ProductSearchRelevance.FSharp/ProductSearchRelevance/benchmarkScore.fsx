#r "../packages/Accord/lib/net40/Accord.dll"
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../packages/Accord.Math/lib/net40/Accord.Math.dll"
#r "../packages/Accord.Statistics/lib/net40/Accord.Statistics.dll"
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Collections.ParallelSeq
open Accord.Statistics.Models.Regression.Linear

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

(* wordMatch needs to have a fuzzy match
   The sample R script uses a regex like this:
   pattern <- paste("(^| )",words[i],"($| )",sep="") *)
let wordMatch (words:string) (title:string) (desc:string) =
    let isMatch input word = // TODO should stem words, e.g. "angles" -> "angle"
        Regex.IsMatch(input, sprintf @"\b%s\b" (Regex.Escape word), RegexOptions.IgnoreCase)
    let words' = words.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let uniqueWords = words' |> Array.distinct
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

let target = MultipleLinearRegression(3, true);
let error = target.Regress(trainInput, trainOutput);
let testOutput = target.Compute(testInput)

let submission = 
    Seq.zip test.Rows testOutput
    |> PSeq.map (fun (r,o) -> sprintf "%A,%A" r.Id o)
    |> PSeq.toArray

let outputPath = __SOURCE_DIRECTORY__ + @"../../data/benchmark_submission_FSharp.csv"
File.WriteAllLines(outputPath, submission)
