#I "../packages/"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "StemmersNet/lib/net20/StemmersNet.dll"
#r "FuzzyString/lib/FuzzyString.dll"
#load "CsvData.fs"
#load "StringUtils.fs"
#load "TFIDF.fs"

open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent
open TFIDF

let trainSamples = CsvData.getTrainSamples()
let testSamples = CsvData.getTestSamples()
let allSamples = Seq.append trainSamples testSamples

let termFrequencies = 
  allSamples
  |> PSeq.map getTermFrequencies
  |> PSeq.toArray

let termDocumentFreqDict = ConcurrentDictionary<TermToken, int>()
let incrementDocFreq token = termDocumentFreqDict.AddOrUpdate(token, 1, (fun _ o -> o + 1)) |> ignore
let processDocument = Seq.map fst >> Seq.iter incrementDocFreq

termFrequencies |> PSeq.iter processDocument

let mostPopular = 
  termDocumentFreqDict
  |> PSeq.sortBy (fun (KeyValue(k, v)) -> v * -1)
  |> Seq.take 100
  |> Seq.iter (fun (KeyValue(k, v)) -> printf "\"%s\"; " (tokenToTerm k))

let sampleCount = termFrequencies.Length

let tfidfs = 
  termDocumentFreqDict
  |> PSeq.map (fun (KeyValue(k, v)) -> k, float v * inverseDocumentFrequency sampleCount v)
  |> PSeq.sortBy (fun (k, v) -> v)
  |> Seq.take 100
  |> Seq.iter (fun (k, v) -> printfn "%s : %f" (tokenToTerm k) v)
