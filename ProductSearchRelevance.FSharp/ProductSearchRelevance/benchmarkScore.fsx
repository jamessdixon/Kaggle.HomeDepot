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
open Accord.Statistics.Models.Regression.Linear
open Iveonik.Stemmers
open FSharp.Collections.ParallelSeq

type Train = CsvProvider<"../data/train.csv">
let train = Train.GetSample()

type Test = CsvProvider<"../data/test.csv">
let test = Test.GetSample()

type Products = CsvProvider<"../data/product_descriptions.csv">
let products = Products.GetSample()

type Attributes = CsvProvider<"../data/attributes.csv">
let attributes = Attributes.GetSample()

let productDescMap =
    products.Rows
    |> Seq.map (fun pd -> pd.Product_uid, pd.Product_description)
    |> Map.ofSeq
let inline productDescription uid = productDescMap |> Map.find uid

let attribMap =
    attributes.Rows
    |> Seq.map (fun r -> r.Product_uid, r.Name, r.Value)
    |> Seq.groupBy (fun (i,_,_) -> i)
    |> Map.ofSeq

let brands =
  attributes.Rows
  |> Seq.where (fun r -> r.Name = "MFG Brand Name")
  |> Seq.map (fun r -> r.Value.ToLowerInvariant().Replace(" & ", " and ").Replace(".", "").Replace("'", "").Replace("-", " ").Trim()) // TODO better sanitize
  |> Set.ofSeq

let attribs uid =
    match attribMap |> Map.tryFind uid with
    | Some a ->
      let getAttrStr name (value:string) =
          match value.ToLowerInvariant() with
          | "yes" -> name // if true attrib, include attrib name
          | "no"  -> String.Empty
          | _     -> value
      a |> Seq.map (fun (_, name, value) -> getAttrStr name value) |> String.concat " "
    | None -> String.Empty

let brandName uid =
    match attribMap |> Map.tryFind uid with
    | Some a ->
      let brand = a |> Seq.tryFind (fun (_, name, _) -> name = "MFG Brand Name")
      brand |> Option.map (fun (_, _, value) -> value)
    | None -> None

let stem word =
    let stemmer = EnglishStemmer() // NOTE stemmer not thread-safe
    let stemmed = stemmer.Stem word
    if stemmed.Length < word.Length then stemmed // HACK workaround stemmer output like "vanity" -> "vaniti"
    else word

let containedIn (input:string) word =
    input.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0

let features isMatch (words:string) title desc attribs productBrand =
    let uniqueWords = words.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.distinct
    let titleMatches = uniqueWords |> Array.filter (isMatch title)
    let descMatches = uniqueWords |> Array.filter (isMatch desc)
    let attrMatches = uniqueWords |> Array.filter (isMatch attribs)
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
    [| float titleMatches.Length
       float descMatches.Length
       float attrMatches.Length
       float uniqueWords.Length
       float wordMatchCount
       float brandNameMatch |]

let isStemmedMatch input word =
    let word' = stem word |> Regex.Escape // TODO strip punctuation?
    Regex.IsMatch(input, sprintf @"\b%s" word', RegexOptions.IgnoreCase)
let stemmedWordMatch = features isStemmedMatch

let stringContainsMatch = features containedIn

let trainInput = 
    train.Rows
    |> Seq.map (fun w ->
        stemmedWordMatch
          w.Search_term
          w.Product_title
          (productDescription w.Product_uid)
          (attribs w.Product_uid)
          (brandName w.Product_uid))
    |> Seq.toArray

let trainOutput = 
    train.Rows
    |> Seq.map (fun t -> float t.Relevance)
    |> Seq.toArray

let target = MultipleLinearRegression(6, true)
let sumOfSquaredErrors = target.Regress(trainInput, trainOutput)
let observationCount = trainInput |> Seq.length |> float
let sme = sumOfSquaredErrors / observationCount
let rsme = sqrt(sme)
//0.49359 - better brand name matching
//0.49409 - attributes + some brand matching
//0.49665 - stemmed
//0.50783 - kaggle reported from stemmed

//0.5063 - string contains

let testInput = 
    test.Rows 
    |> Seq.map (fun w ->
        stemmedWordMatch
          w.Search_term
          w.Product_title
          (productDescription w.Product_uid)
          (attribs w.Product_uid)
          (brandName w.Product_uid))
    |> Seq.toArray

let testOutput = target.Compute(testInput)
let inline bracket n = Math.Max(1., Math.Min(3., n))
let testOutput' = testOutput |> Seq.map bracket

let submission = 
    Seq.zip test.Rows testOutput'
    |> Seq.map (fun (r,o) -> sprintf "%A,%A" r.Id o)
    |> Seq.toList
let outputPath = __SOURCE_DIRECTORY__ + @"../../data/benchmark_submission_FSharp.csv"
File.WriteAllLines(outputPath, "id,relevance" :: submission)
