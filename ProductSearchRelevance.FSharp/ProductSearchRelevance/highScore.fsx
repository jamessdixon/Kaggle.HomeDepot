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
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq
open HomeDepot.Core
open StringUtils
open System.Text

printfn "Building Brand Name set..."
let brands =
    CsvData.attributes.Rows
    |> Seq.where (fun r -> r.Name = "MFG Brand Name")
    |> Seq.map (fun r -> r.Value.ToLowerInvariant())
    |> Set.ofSeq

let brandName attribMap uid =
    match attribMap |> Map.tryFind uid with
    | Some a ->
      let brand = a |> Seq.tryFind (fun (_, name, _) -> name = "MFG Brand Name")
      brand |> Option.map (fun (_, _, value) -> value)
    | None -> None

let isMatch input word =
    let word' = word |> sanitize |> stem |> Regex.Escape
    Regex.IsMatch(input |> sanitize |> stemWords, sprintf @"\b%s" word', RegexOptions.IgnoreCase)

let fixLineConcats (attribs:string seq) (desc:string) =
    attribs
    |> Seq.where (String.IsNullOrWhiteSpace >> not)
    |> Seq.fold
        (fun (state:StringBuilder) t -> state.Replace(t, t + " "))
        (StringBuilder(desc))
    |> string

let attributesConcat attribs =
    let getAttrStr name (value:string) =
        match value.ToLowerInvariant() with
        | "yes" -> name // if true attrib, include attrib name
        | "no"  -> String.Empty
        | _     ->
          match name with
          | b when b.StartsWith "Bullet" -> value
          | _ -> name + " " + value
    attribs
    |> Seq.map (fun (k,v) -> getAttrStr k v)

let attributeNames (attribs:(string * string) seq) =
    attribs |> Seq.where (fun (k,_) -> k.StartsWith "Bullet" |> not) |> Seq.map fst

let features attrSelector productBrand (sample:CsvData.Sample) =
    let words = sample.Query
    let uniqueWords = words |> splitOnSpaces |> Array.distinct
    let title = sample.Title
    let titleWords = title |> splitOnSpaces

    let indices = uniqueWords |> Seq.map (fun w -> titleWords |> Seq.tryFindIndexBack (isMatch w)) |> Seq.choose id
    let queryTitleIndices = indices |> Seq.map (fun i -> float i / float titleWords.Length) |> Array.ofSeq
    let queryTitlePosScore = if Array.isEmpty queryTitleIndices then 0. else Seq.average queryTitleIndices

    let attributes = attrSelector sample.ProductId |> Array.ofSeq
    let attributeNames = attributeNames attributes
    let attrNameMatches = attributeNames |> Seq.filter (fun n -> uniqueWords |> Seq.exists (isMatch n))
    let attributesText = attributesConcat attributes
    let desc = sample.Description |> fixLineConcats attributesText
    let deduped = attributesText |> Seq.where (containedIn desc >> not) |> String.concat " "

    let titleMatches = uniqueWords |> Array.filter (isMatch title)
    let descMatches = uniqueWords |> Array.filter (isMatch desc)
    let attrMatches = uniqueWords |> Array.filter (isMatch deduped)

    let uniqueWordsBwd = uniqueWords |> Seq.rev
    let titleWordsBwd = titleWords |> Seq.rev
    let bwd = Seq.zip uniqueWordsBwd titleWordsBwd |> Seq.takeWhile (fun (u,t) -> u |> isMatch t) |> Seq.length
    let fwd = Seq.zip uniqueWords titleWords |> Seq.takeWhile (fun (u,t) -> u |> isMatch t) |> Seq.length

    let lastWordMatch = (uniqueWords |> Array.last) |> isMatch (titleWords |> Array.last)
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
       float deduped.Length
       float wordMatchCount
       float titleMatches.Length
       float titleMatches.Length / float uniqueWords.Length
       float descMatches.Length
       float descMatches.Length / float uniqueWords.Length
       float attrMatches.Length
       (if attributes.Length > 0 then 1. else 0.)
       (if lastWordMatch then 1. else 0.)
       float fwd
       float bwd
       float (attrNameMatches |> Seq.length)
       queryTitlePosScore
//       float attrMatches.Length / float uniqueWords.Length
       float brandNameMatch |]

let attributes attribMap productId =
    match attribMap |> Map.tryFind productId with
    | Some a -> a |> Seq.map (fun (_,k,v) -> k, v)
    | None -> Seq.empty

let getFeatures attribs attribMap sample =
    sample |> features attribs (brandName attribMap sample.ProductId)

let extractFeatures featureExtractor = 
    PSeq.ordered
    >> PSeq.map featureExtractor
    >> PSeq.toArray

let rfLearn (examples:Example array) attribMap =
  let samples, trainOutput = Array.unzip examples

  printfn "Extracting training features..."
  let attribs = attributes attribMap
  let getFeatures' = getFeatures attribs attribMap
  let trainInput = samples |> extractFeatures getFeatures'
  // NOTE: ALGLIB wants prediction variable at end of input array
  let trainInputOutput =
      Seq.zip trainInput trainOutput
      |> Seq.map (fun (i,o) -> Array.append i [|o|])
      |> array2D

  printfn "Random Decision Forest regression..."
  let trees = 600
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

//let rfQuality = evaluate rfLearn
submission rfLearn
//0.48737 = kaggle rsme; RDF RMS Error: 0.430396; Out-of-bag RMS Error: 0.477678
//0.48371 = kaggle rsme; RDF RMS Error: 0.451875; Out-of-bag RMS Error: 0.475592
//0.47806 = kaggle rsme; RDF RMS Error: 0.446892; Out-of-bag RMS Error: 0.470399
//0.47774 = kaggle rsme; RDF RMS Error: 0.446649; Out-of-bag RMS Error: 0.470098
//?.????? = kaggle rsme; RDF RMS Error: 0.446260; Out-of-bag RMS Error: 0.469681 : attr text len
//?.????? = kaggle rsme; RDF RMS Error: 0.445838; Out-of-bag RMS Error: 0.469287 : 600 trees
//?.????? = kaggle rsme; RDF RMS Error: 0.433009; Out-of-bag RMS Error: 0.468017 : 600 trees/7.5% bag
//0.47743 = kaggle rsme; RDF RMS Error: 0.420040; Out-of-bag RMS Error: 0.466575 : 600 trees/10% bag
//0.47629 = kaggle rsme; RDF RMS Error: 0.419235; Out-of-bag RMS Error: 0.465694 : last word match
//?.????? = kaggle rsme; RDF RMS Error: 0.419149; Out-of-bag RMS Error: 0.465570 : longest sequence of query + title matching tail words 1933
//0.47552 = kaggle rsme; RDF RMS Error: 0.418375; Out-of-bag RMS Error: 0.464791 : longest sequence of query + title matching lead words 2041
