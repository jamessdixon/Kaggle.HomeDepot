#I @"../packages/"

#r "Accord/lib/net45/Accord.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Accord.Math/lib/net45/Accord.Math.dll"
#r "Accord.Statistics/lib/net45/Accord.Statistics.dll"
#r "FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "StemmersNet/lib/net20/StemmersNet.dll"
#r "alglibnet2/lib/alglibnet2.dll"
#r "FuzzyString/lib/FuzzyString.dll"
#load "CsvData.fs"
#load "StringUtils.fs"
#load "Core.fs"
#load "TFIDF.fs"

open System
open FSharp.Collections.ParallelSeq
open StringUtils
open Accord.Statistics.Models.Regression.Linear
open HomeDepot.Core
open HomeDepot.TFIDF
open Accord.Statistics.Analysis

printfn "Building Brand Name set..."
let brands =
    CsvData.attributes.Rows
    |> Seq.where (fun r -> r.Name = "MFG Brand Name")
    |> Seq.map (fun r -> r.Value |> prepareText)
    |> Set.ofSeq

let binary = function true -> 1. | _ -> 0.
let intersect a b = Set.intersect (Set.ofSeq a) (Set.ofSeq b)
let fuzzyIntersect a b =
  a |> Seq.filter (fun a -> b |> Seq.exists (fun b -> isFuzzyMatch a b))
let overlap' a b = intersect a b |> Set.count
let overlap a b =
  match overlap' a b with
  | 0 -> fuzzyIntersect a b  |> Seq.length
  | o -> o

let features attrSelector productBrand (sample:CsvData.Sample) =
    let queryWords = sample.Query |> prepareText
    let titleWords = sample.Title |> prepareText
    let descWords = sample.Description |> prepareText
    let titleMatches = queryWords |> overlap titleWords
    let descMatches = queryWords |> overlap descWords
//    let queryInTitle = sample.Query |> containedIn sample.Title
//    let queryInDescription = sample.Query |> containedIn sample.Description
//    let queryBigrams = nGrams 2 queryWords |> Array.ofSeq
//    let titleBiMatches = nGrams 2 titleWords |> overlap' queryBigrams
//    let descBiMatches = nGrams 2 descWords |> overlap' queryBigrams
//    let attributes = prepareText <| attrSelector sample.ProductId
//    let attrMatches = queryWords |> Array.filter (isMatch attributes)
//    let wordMatchCount =
//        queryWords
//        |> Seq.filter (fun w -> Seq.concat [titleMatches; descMatches; attrMatches] |> Seq.contains w)
//        |> Seq.length
    let brandNameMatches =
        match productBrand with // does query contain product brand?
        | Some bn ->
          let brandWords = bn |> splitOnSpaces
          queryWords |> overlap brandWords
        | None ->
          // does query contain any brand name?
          let searchedBrand = brands |> Set.filter (fun b -> overlap' queryWords b > 0) |> Seq.tryHead
          match searchedBrand with // is query brand name in product title?
          | Some b -> overlap' b titleWords
          | None -> 0
    // feature array
    [| float sample.Query.Length
       float sample.Title.Length
       float sample.Description.Length
//       float queryWords.Length
//       float titleWords.Length
//       float descWords.Length
       float titleMatches
       float titleMatches / float queryWords.Length
//       float titleBiMatches
//       float descBiMatches
       float descMatches
       float descMatches / float queryWords.Length
//       binary queryInTitle
//       binary queryInDescription
       float brandNameMatches
       float brandNameMatches / float queryWords.Length |]

let getAttr attribMap productId =
    match attribMap |> Map.tryFind productId with
    | Some a ->
      let getAttrStr name (value:string) =
          match value.ToLowerInvariant() with
          | "yes" -> name // if true attrib, include attrib name
          | "no"  -> String.Empty
          | _     -> value
      a |> Seq.map (fun (_, name, value) -> getAttrStr name value) |> String.concat " "
    | None -> String.Empty

let brandName attribMap uid =
    match attribMap |> Map.tryFind uid with
    | Some a ->
      let brand = a |> Seq.tryFind (fun (_, name, _) -> name = "MFG Brand Name")
      brand |> Option.map (fun (_, _, value) -> value)
    | None -> None

let getFeatures attribs attribMap sample =
    sample |> features attribs (brandName attribMap sample.ProductId)

let extractFeatures featureExtractor = 
    PSeq.ordered
    >> PSeq.map featureExtractor
    >> PSeq.toArray

let rfLearn (examples:Example array) attribMap =
  let samples, trainOutput = Array.unzip examples

  printfn "Extracting training features..."
  let attribs = getAttr attribMap
  let getFeatures' = getFeatures attribs attribMap
  let trainInput = samples |> extractFeatures getFeatures'
  // NOTE: ALGLIB wants prediction variable at end of input array
  let trainInputOutput =
      Seq.zip trainInput trainOutput
      |> Seq.map (fun (i,o) -> Array.append i [|o|])
      |> array2D

  printfn "Random Decision Forest regression..."
  let trees = 300
  let treeTrainSize = 0.1
  let featureCount = trainInput.[0].Length
  let _info, forest, forestReport =
      alglib.dfbuildrandomdecisionforest(trainInputOutput, trainInput.Length, featureCount, 1, trees, treeTrainSize)
  printfn "RDF RMS Error: %f; Out-of-bag RMS Error: %f" forestReport.rmserror forestReport.oobrmserror

  let predict samples =
    let trainInput = samples |> extractFeatures getFeatures'
    let mutable result : float [] = [||]
    trainInput
    |> Array.map (fun features -> 
        alglib.dfprocess(forest, features, &result)
        result.[0])
  predict

let rfQuality = evaluate rfLearn
//0.48737 = kaggle rsme; RDF RMS Error: 0.430396; Out-of-bag RMS Error: 0.477678
//0.48740 = kaggle rsme; RDF RMS Error: 0.430214; Out-of-bag RMS Error: 0.477583
//?.????? = kaggle rsme; RDF RMS Error: 0.430041; Out-of-bag RMS Error: 0.477391

let linLearn (examples:Example array) attribMap =
  let samples, trainOutput = Array.unzip examples

  printfn "Extracting training features..."
  let attribs = getAttr attribMap
  let getFeatures' = getFeatures attribs attribMap
  let trainInput : float [] [] = extractFeatures getFeatures' samples

  printfn "Multiple linear regression..."
  let featureCount = trainInput.[0].Length
  let target = MultipleLinearRegression(featureCount, true)
  let sumOfSquaredErrors = target.Regress(trainInput, trainOutput)
  let observationCount = float trainInput.Length
  let sme = sumOfSquaredErrors / observationCount
  let rsme = sqrt(sme)
  printfn "Linear regression RSME: %f" rsme

  let predict samples =
      let features = extractFeatures getFeatures' samples
      target.Compute(features)
  predict

let linQuality = evaluate linLearn
////0.48754 - string handling tweaks
////0.48835 - sanitize text input
////0.48917 - stem all words for comparison
////0.48940 - added title & desc length feature
////0.49059 - added desc word match ratio
////0.49080 - added title word match ratio
////0.49279 - added query length feature
////0.49359 - better brand matching
////0.49409 - attributes + some brand matching
////0.49665 - stemmed
////0.50783 - kaggle reported from stemmed
////0.5063 - string contains

let logLearn (examples:Example array) attribMap =
  let samples, trainOutput = Array.unzip examples

  printfn "Extracting training features..."
  let attribs = getAttr attribMap
  let getFeatures' = getFeatures attribs attribMap
  let trainInput : float [] [] = extractFeatures getFeatures' samples

  let normalize (min,max) (min',max') i =
    (max' - min') / (max - min) * (i - max) + max'
  let normalizeInput = normalize (1.0, 3.0) (0.0, 1.0)
  let normalizeOutput = normalize (0.0, 1.0) (1.0, 3.0)

  printfn "Logistic regression..."
  let target = LogisticRegressionAnalysis(trainInput, trainOutput |> Array.map normalizeInput)
  target.Compute() |> ignore

  let predict samples =
      let features = extractFeatures getFeatures' samples
      target.Regression.Compute(features) |> Array.map normalizeOutput
  predict

let logQuality = evaluate logLearn
