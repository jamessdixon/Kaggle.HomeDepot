#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open System
open System.IO
open FSharp.Data
open System.Text.RegularExpressions

[<Literal>]
let trainDataPath = "../data/train.csv"
type Train = CsvProvider<trainDataPath>
let train = Train.GetSample().Rows

//Note that the train target variable is an average of at least 3 human raters. It could be more than 3.
let distribution = train |> Seq.countBy(fun r -> r.Relevance)
                         |> Seq.sortBy(fun (r,c) -> r)
                         |> Seq.iter(fun (r,c) -> printfn "%A:%A" r c )
                         
                          