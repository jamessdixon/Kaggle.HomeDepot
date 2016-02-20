#I @"../packages/"
#r "Accord/lib/net40/Accord.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Accord.Math/lib/net40/Accord.Math.dll"
#r "Accord.Statistics/lib/net40/Accord.Statistics.dll"
#r "FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"

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

let productDescription uid = 
    products.Rows 
    |> Seq.tryFind(fun pd -> pd.Product_uid = uid)
    |> Option.map (fun pd -> pd.Product_description)

//Speed up significently
//let productDescMap =
//    products.Rows
//    |> Seq.map (fun pd -> pd.Product_uid, pd.Product_description)
//    |> Map.ofSeq
//let productDescription uid = productDescMap |> Map.find uid

let wordMatch (words:string) (title:string) (desc:string option) =
    let isMatch input word = 
        Regex.IsMatch(input, sprintf @"\b%s\b" (Regex.Escape word), RegexOptions.IgnoreCase)
    let words' = words.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let uniqueWords = words' |> Array.distinct
    let numberInTitle = uniqueWords |> Array.filter (isMatch title) |> Array.length
    let numberInDescription =
        match desc with
        | Some desc -> uniqueWords |> Array.filter (isMatch desc) |> Array.length
        | None -> 0
    numberInTitle, numberInDescription, uniqueWords.Length 

let trainInput = 
    train.Rows 
    |> Seq.map (fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> Seq.map (fun (t,d,w) -> [| float t; float d; float w |])
    |> Seq.toArray

let trainOutput = 
    train.Rows
    |> Seq.map (fun t -> float t.Relevance)
    |> Seq.toArray

let testInput = 
    test.Rows 
    |> Seq.map (fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> Seq.map (fun (t,d,w) -> [| float t; float d; float w |])
    |> Seq.toArray

let target = MultipleLinearRegression(3, true);
let error = target.Regress(trainInput, trainOutput);
let testOutput = target.Compute(testInput)

let submission = 
    Seq.zip test.Rows testOutput
    |> Seq.map (fun (r,o) -> sprintf "%A,%A" r.Id o)
    |> Seq.toArray

let outputPath = __SOURCE_DIRECTORY__ + @"../../data/benchmark_submission_FSharp.csv"
File.WriteAllLines(outputPath, submission)

