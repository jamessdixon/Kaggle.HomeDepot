
#r "../packages/Accord/lib/net40/Accord.dll"
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../packages/Accord.Math/lib/net40/Accord.Math.dll"
#r "../packages/Accord.Statistics/lib/net40/Accord.Statistics.dll"
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"

open System
open System.IO
open FSharp.Data
open Accord.Math
open Accord.Statistics
open Accord.Statistics.Links
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
    let pd = 
        products.Rows 
        |> Seq.tryFind(fun pd -> pd.Product_uid = uid)
    if pd.IsSome then
        Some pd.Value.Product_description
    else None

//WordMatch needs to have a fuzzy match
//The sample R script uses a regex like this:
//pattern <- paste("(^| )",words[i],"($| )",sep="")
//I assume we can dup using .NET Reg Ex class like this:
//let pattern = @"\b(\w+)\s\1\b";
//let rgx = new Regex(pattern, RegexOptions.IgnoreCase);
//let matches = rgx.Matches(input)
//Until then, it does an exact match

let wordMatch (words:string) (title:string) (desc:option<string>) =
    let words' = words.Split(' ')
    let uniqueWords = words' |> Seq.distinct
    let numberOfWords = uniqueWords |> Seq.length
    let numberInTitle =  uniqueWords |> Seq.filter(fun w -> title.ToLower().Contains(w.ToLower())) |> Seq.length
    let numberInDescription =
        if desc.IsNone then 0
        else uniqueWords |> Seq.filter(fun w -> desc.Value.ToLower().Contains(w.ToLower())) |> Seq.length
    numberInTitle,numberInDescription,numberOfWords

let trainInput = 
    train.Rows 
    |> PSeq.map(fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> PSeq.map(fun (t,d,w) -> [|(float)t;(float)d;(float)w|])
    |> PSeq.toArray

let trainOutput = 
    train.Rows
    |> PSeq.map(fun t -> (float)t.Relevance)
    |> PSeq.toArray

let testInput = 
    test.Rows 
    |> PSeq.map(fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> PSeq.map(fun (t,d,w) -> [|(float)t;(float)d;(float)w|])
    |> PSeq.toArray

let target = new MultipleLinearRegression(3, true);
let error = target.Regress(trainInput, trainOutput);
let testOutput = target.Compute(testInput)

let submission = 
    Seq.zip test.Rows testOutput
    |> Seq.map(fun (r,o) -> r.Id.ToString(), o.ToString())
    |> Seq.map(fun (id,o) -> String.Format("{0},{1}",id, o))
    |> Seq.toArray

let outputPath = __SOURCE_DIRECTORY__ + @"..\..\data\benchmark_submission_FSharp.csv"
File.WriteAllLines(outputPath,submission)



