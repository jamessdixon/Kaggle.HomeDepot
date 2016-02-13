#I @"../packages/"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#load "CsvData.fs"

// NOTE the train target variable is an average of at least 3 human raters. It could be more than 3.

let distribution = 
    CsvData.train.Rows
    |> Seq.countBy (fun r -> r.Relevance)
    |> Seq.sortBy fst
    |> Seq.iter (fun (r,c) -> printfn "%A:%A" r c )

// is the sample reasonably evenly distributed?
// checking to see how smart we need to be for cross-validation

let size = CsvData.train.Rows |> Seq.length
CsvData.train.Rows |> Seq.take (size/2) |> Seq.averageBy (fun x -> x.Relevance)  
CsvData.train.Rows |> Seq.skip (size/2) |> Seq.averageBy (fun x -> x.Relevance)             
           
let trainIds = CsvData.train.Rows |> Seq.map (fun r -> r.Product_uid) |> Set.ofSeq // 54667 distinct products in train
let testIds = CsvData.test.Rows |> Seq.map (fun r -> r.Product_uid) |> Set.ofSeq // 97460 distinct products in test
let productIds = Set.union trainIds testIds // 124428 distinct products in train + test
let overlap = Set.intersect trainIds testIds // 27699 products appear in both train & test
let trainIdsOnly = Set.difference trainIds testIds // 26968 products appear only in train
let testIdsOnly = Set.difference testIds trainIds // 69761 products appear only in test

let attributes = CsvData.getAttributeMap() // 86263 products with attributes

let attributeCount = CsvData.attributes.Rows |> Seq.length // 2044648 attributes
let attributeKeys =
  CsvData.attributes.Rows
  |> Seq.map (fun r -> r.Name.ToLowerInvariant())
  |> Seq.distinct
  |> Seq.sort // 5343 distinct attribute names
let attributeValues =
  CsvData.attributes.Rows
  |> Seq.map (fun r -> r.Value.ToLowerInvariant())
  |> Seq.distinct
  |> Seq.sort // 301744 distinct attribute values

let trainQueries = CsvData.train.Rows |> Seq.map (fun r -> r.Search_term.ToLowerInvariant()) |> Set.ofSeq
let testQueries = CsvData.test.Rows |> Seq.map (fun r -> r.Search_term.ToLowerInvariant()) |> Set.ofSeq
let queries = Set.union trainQueries testQueries // 24601 distinct queries in train + test

// look for queries with dimensions
open System.Text.RegularExpressions
let dimensionRegex = Regex(@"(\b\d{1,3}(?:\.\d{1,3})?\b(?:\s*(x|by)\s*)?)+", RegexOptions.IgnoreCase)
let dimQueries = queries |> Set.filter dimensionRegex.IsMatch

let trainTitles = CsvData.train.Rows |> Seq.map (fun r -> r.Product_title, r.Search_term)
let lastWord (s:string) = s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.last
let sameEnds = trainTitles |> Seq.filter (fun (t,s) -> lastWord t = lastWord s)
