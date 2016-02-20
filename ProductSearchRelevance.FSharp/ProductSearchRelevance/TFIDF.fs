namespace HomeDepot

module TFIDF =

    open HomeDepot
    open FSharp.Collections.ParallelSeq
    open StringUtils
    open System
    open System.Collections.Concurrent

    let nGrams n words = 
      words
      |> Seq.windowed n
      |> Seq.map join

    type TermToken = int

    type TermFrequency = TermToken * int

    type TFIDF = TermToken * float

    type Document = TermFrequency array

    let termTokenDict = ConcurrentDictionary<string, int>()
    let mutable tokenCount = 0
    let tokenize term : TermToken = termTokenDict.GetOrAdd(term, (fun _ -> tokenCount <- tokenCount + 1; tokenCount))

    let tokenToTerm token = 
      let (KeyValue kv) = termTokenDict |> Seq.find (fun (KeyValue kv) -> snd kv = token)
      fst kv

    let getTermFrequencies (sample:CsvData.Sample) = 
      let document = [ sample.Title; sample.Description ] |> String.concat " "
  
      let grams = 
        nGrams 1 (document.ToLowerInvariant() |> splitOnSpaces)
        |> Seq.map tokenize
        |> Array.ofSeq
  
      let gramSet = Set.ofArray grams
  
      let frequency g = 
        g, 
        grams
        |> Seq.where (fun w -> w = g)
        |> Seq.length
      gramSet
      |> Seq.map frequency
      |> Array.ofSeq

    let getDocumentFrequency (documents : Document array) term = 
      documents
      |> PSeq.filter (Array.exists (fun (t, _) -> t = term))
      |> PSeq.length

    let inverseDocumentFrequency sampleCount docFrequency = Math.Log(float sampleCount / (float docFrequency + 1.0))
