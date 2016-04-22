// this script can be used to train a word2vec model

#I "../packages/"

#r @"FSharp.Data/lib/net40/FSharp.Data.dll"
#r @"StemmersNet/lib/net20/StemmersNet.dll"
#r @"FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"

#load "Utilities.fs"

open FSharp.Data
type Descriptions = CsvProvider<"../cache/product_descriptions.csv">
let sample = Descriptions.GetSample().Rows |> Seq.map (fun r -> r.Product_description)

open System
open System.IO
open HomeDepot.Utilities
open FSharp.Collections.ParallelSeq

let (@@) a b = Path.Combine(a,b)

let kagglePath = Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory) @@ "Kaggle"

let path = kagglePath @@ "descriptions.txt"
let descriptions = sample |> PSeq.map preprocess
let file = System.IO.File.WriteAllLines(path, descriptions)

#r @"../../../Word2Vec/Word2Vec.Net/bin/Release/Word2Vec.Net.dll" // NOTE you must build this locally, path may differ
open Word2Vec

let output = kagglePath @@ "output.bin"
let vocab = kagglePath @@ "vocab.txt"
let builder = 
    Word2Vec.Net.Word2VecBuilder
        .Create()
        .WithTrainFile(path)
        .WithOutputFile(output)
        .WithBinary(1)
//        .WithCBow(1)
//        .WithSize(50)
        .WithSaveVocubFile(vocab)
        .WithHs(1)
        .WithIter(10)
//        .WithWindow(5)
        .Build()

builder.TrainModel()

fsi.AddPrinter(fun (bw:Net.BestWord) -> sprintf "%s:%A" bw.Word bw.Distance)

let distance = Word2Vec.Net.Distance(output)
distance.Search("wood")

let analogy = Word2Vec.Net.WordAnalogy(output)
analogy.Search("chrome steel plastic")
