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
open System.Collections.Concurrent

type Train = CsvProvider<"../data/train.csv">
let train = Train.GetSample()

type Test = CsvProvider<"../data/test.csv">
let test = Test.GetSample()

type Products = CsvProvider<"../data/product_descriptions.csv">
let products = Products.GetSample()

type Attributes = CsvProvider<"../data/attributes.csv">
let attributes = Attributes.GetSample()

printfn "Building Product Description map..."
let productDescMap =
    products.Rows
    |> Seq.map (fun pd -> pd.Product_uid, pd.Product_description)
    |> Map.ofSeq
let inline productDescription uid = productDescMap |> Map.find uid

printfn "Building Product Attribute map..."
let attribMap =
    attributes.Rows
    |> Seq.map (fun r -> r.Product_uid, r.Name, r.Value)
    |> Seq.groupBy (fun (i,_,_) -> i)
    |> Map.ofSeq

let sanitize (str:string) =
  let clean = System.Text.StringBuilder(str.Length)
  for char in str do
    match char with
    | c when Char.IsLetterOrDigit c || Char.IsWhiteSpace c ->
      clean.Append char |> ignore
    | '-' | '/' ->
      clean.Append " " |> ignore
    | _ -> ()
  clean.ToString().TrimEnd()

printfn "Building Brand Name set..."
let brands =
  attributes.Rows
  |> Seq.where (fun r -> r.Name = "MFG Brand Name")
  |> Seq.map (fun r -> r.Value.ToLowerInvariant().Replace(" & ", " and ") |> sanitize)
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

let stemDict = ConcurrentDictionary<string,string>(StringComparer.OrdinalIgnoreCase)
let stem word = stemDict.GetOrAdd(word, (fun s -> (new EnglishStemmer()).Stem s))
let splitOnSpaces (str:string) = str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
let stemWords = splitOnSpaces >> Array.map stem >> String.concat " "

let containedIn (input:string) word =
    input.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0

let features isMatch (words:string) title desc attribs productBrand =
    let uniqueWords = words |> splitOnSpaces |> Array.distinct
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

let isStemmedMatch input word =
    let word' = word |> sanitize |> stem |> Regex.Escape // TODO strip punctuation?
    Regex.IsMatch(input |> sanitize |> stemWords, sprintf @"\b%s" word', RegexOptions.IgnoreCase)
let stemmedWordMatch = features isStemmedMatch

let stringContainsMatch = features containedIn

printfn "Extracting training features..."
let trainInput = 
    train.Rows
    |> PSeq.ordered
    |> PSeq.map (fun w ->
        stemmedWordMatch
          w.Search_term
          w.Product_title
          (productDescription w.Product_uid)
          (attribs w.Product_uid)
          (brandName w.Product_uid))
    |> PSeq.toArray

let trainOutput = 
    train.Rows
    |> Seq.map (fun t -> float t.Relevance)
    |> Seq.toArray

printfn "Regressing..."
let target = MultipleLinearRegression(11, true)
let sumOfSquaredErrors = target.Regress(trainInput, trainOutput)
let observationCount = trainInput |> Seq.length |> float
let sme = sumOfSquaredErrors / observationCount
let rsme = sqrt(sme)
//0.48835 - sanitize text input
//0.48917 - stem all words for comparison
//0.48940 - added title & desc length feature
//0.49059 - added desc word match ratio
//0.49080 - added title word match ratio
//0.49279 - added query length feature
//0.49359 - better brand matching
//0.49409 - attributes + some brand matching
//0.49665 - stemmed
//0.50783 - kaggle reported from stemmed
//0.5063 - string contains

printfn "Extracting testing features..."
let testInput = 
    test.Rows
    |> PSeq.ordered
    |> PSeq.map (fun w ->
        stemmedWordMatch
          w.Search_term
          w.Product_title
          (productDescription w.Product_uid)
          (attribs w.Product_uid)
          (brandName w.Product_uid))
    |> PSeq.toArray

printfn "Predicting..."
let testOutput = target.Compute(testInput)
let inline bracket n = Math.Max(1., Math.Min(3., n))
let testOutput' = testOutput |> Seq.map bracket

printfn "Writing results..."
let submission = 
    Seq.zip test.Rows testOutput
    |> PSeq.ordered
    |> PSeq.map (fun (r,o) -> sprintf "%A,%A" r.Id o)
    |> PSeq.toList
let outputPath = __SOURCE_DIRECTORY__ + @"../../data/benchmark_submission_FSharp.csv"
File.WriteAllLines(outputPath, "id,relevance" :: submission)
