#I @"../packages/"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"

open System
open System.IO
open FSharp.Data
open System.Text.RegularExpressions

[<Literal>]
let trainDataPath = "../data/train.csv"
type Train = CsvProvider<trainDataPath>
let train = Train.GetSample().Rows

//Note that the train target variable is an average of at least 3 human raters. It could be more than 3.

let distribution = 
    train 
    |> Seq.countBy(fun r -> r.Relevance)
    |> Seq.sortBy(fun (r,c) -> r)
    |> Seq.iter(fun (r,c) -> printfn "%A:%A" r c )

// is the sample reasonably evenly distributed?
// checking to see how smart we need to be for cross-validation

let size = train |> Seq.length

train |> Seq.take (size/2) |> Seq.averageBy (fun x -> x.Relevance)  
train |> Seq.skip (size/2) |> Seq.averageBy (fun x -> x.Relevance)             
           
                          