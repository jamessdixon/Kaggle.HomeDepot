#r "../packages/Accord/lib/net45/Accord.dll"
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../packages/Accord.Math/lib/net45/Accord.Math.dll"
#r "../packages/Accord.Statistics/lib/net45/Accord.Statistics.dll"
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "../packages/StemmersNet/lib/net20/StemmersNet.dll"
#r "../packages/alglibnet2/lib/alglibnet2.dll"
#r "../packages/FuzzyString/lib/FuzzyString.dll"
#load "StringUtils.fs"

open System
open System.IO
open FSharp.Data
open FSharp.Collections.ParallelSeq
open StringUtils
open Accord.Statistics.Models.Regression.Linear

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

printfn "Building Brand Name set..."
let brands =
  attributes.Rows
  |> Seq.where (fun r -> r.Name = "MFG Brand Name")
  |> Seq.map (fun r -> r.Value.ToLowerInvariant().Replace(" & ", " and ") |> sanitize)
  |> Set.ofSeq

type Sample = {
    Id : int
    ProductId : int
    Title : string
    Description : string
    Attributes : string
    Query : string }
let sample id productId title query =
    { Id = id
      ProductId = productId
      Title = prepareText title
      Description = prepareText <| productDescription productId
      Attributes = prepareText <| attribs productId
      Query = prepareText query }  

let trainSamples =
    train.Rows |> Seq.map (fun r -> sample r.Id r.Product_uid r.Product_title r.Search_term)

let brandName uid =
    match attribMap |> Map.tryFind uid with
    | Some a ->
      let brand = a |> Seq.tryFind (fun (_, name, _) -> name = "MFG Brand Name")
      brand |> Option.map (fun (_, _, value) -> value)
    | None -> None

let isMatch = startsWithMatch
let features sample productBrand =
    let queryWords = sample.Query |> splitOnSpaces |> Array.distinct
    let titleMatches = queryWords |> Array.filter (isMatch sample.Title)
    let descMatches = queryWords |> Array.filter (isMatch sample.Description)
    let attrMatches = queryWords |> Array.filter (isMatch sample.Attributes)
    let wordMatchCount =
        queryWords
        |> Seq.filter (fun w -> Seq.concat [titleMatches; descMatches; attrMatches] |> Seq.distinct |> Seq.contains w)
        |> Seq.length
    let brandNameMatch =
        match productBrand with // does query contain product brand?
        | Some bn -> if queryWords |> Array.exists (containedIn bn) then 1 else 0
        | None ->
          // does query contain any brand name?
          let searchedBrand = brands |> Set.filter (containedIn sample.Query) |> Seq.tryHead
          match searchedBrand with // is query brand name in product title?
          | Some b -> if b |> containedIn sample.Title then 1 else -1
          | None -> 0
    [| float queryWords.Length
       float sample.Query.Length
       float sample.Title.Length
       float sample.Description.Length
       float wordMatchCount
       float titleMatches.Length
       float titleMatches.Length / float queryWords.Length
       float descMatches.Length / float queryWords.Length
       float descMatches.Length
       float attrMatches.Length
       float brandNameMatch |]

let getFeatures samples =
    samples
    |> PSeq.ordered
    |> PSeq.map (fun s -> features s (brandName s.Id))
    |> PSeq.toArray

printfn "Extracting training features..."
let trainInput = getFeatures trainSamples
let trainOutput = 
    train.Rows
    |> Seq.map (fun t -> float t.Relevance)
    |> Array.ofSeq
let trainInputOutput = // NOTE: ALGLIB wants prediction variable at end of input array
  Seq.zip trainInput trainOutput
  |> Seq.map (fun (i,o) -> Array.append i [|o|])
  |> array2D

let featureCount = trainInput.[0].Length

printfn "Random Decision Forest regression..."
let trees = 75
let treeTrainSize = 0.1
let info, f, r =
  alglib.dfbuildrandomdecisionforest(trainInputOutput, trainInput.Length, featureCount, 1, trees, treeTrainSize)
printfn "RDF RMS Error: %f; Out-of-bag RMS Error: %f" r.rmserror r.oobrmserror
//0.48737 = kaggle rsme; rmserror = 0.4303968628; oobrmserror = 0.4776784128
//0.48740 = kaggle rsme; RDF RMS Error: 0.430214; Out-of-bag RMS Error: 0.477583

printfn "Multiple linear regression..."
let target = MultipleLinearRegression(featureCount, true)
let sumOfSquaredErrors = target.Regress(trainInput, trainOutput)
let observationCount = float trainInput.Length
let sme = sumOfSquaredErrors / observationCount
let rsme = sqrt(sme)
printfn "Linear regression RSME: %f" rsme
//0.48754 - string handling tweaks
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
let testSamples =
    test.Rows |> Seq.map (fun r -> sample r.Id r.Product_uid r.Product_title r.Search_term)
let testInput = getFeatures testSamples

let writeResults name rows =
    let outputPath = __SOURCE_DIRECTORY__ + sprintf "../../data/%s_submission_FSharp.csv" name
    File.WriteAllLines(outputPath, "id,relevance" :: rows)  

let inline bracket n = Math.Max(1., Math.Min(3., n)) // ensure output between 1..3

printfn "Predicting with random forest..."
let mutable result : float [] = [||]
let rfSubmission =
    testSamples
    |> Seq.mapi (fun i r ->
        alglib.dfprocess(f, testInput.[i], &result) 
        sprintf "%A,%A" r.Id (bracket result.[0]))
    |> List.ofSeq
    |> writeResults "rf"

printfn "Predicting with linear regression..."
let testOutput = target.Compute(testInput)
let testOutput' = testOutput |> Seq.map bracket

let linRegSubmission = 
    Seq.zip test.Rows testOutput
    |> PSeq.ordered
    |> PSeq.map (fun (r,o) -> sprintf "%A,%A" r.Id o)
    |> PSeq.toList
    |> writeResults "linear"
