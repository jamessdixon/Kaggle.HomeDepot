#I "../packages/"

#r @"FSharp.Data/lib/net40/FSharp.Data.dll"
#r @"StemmersNet/lib/net20/StemmersNet.dll"
#r @"FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"

#load "Utilities.fs"

open FSharp.Data

//[<Literal>]
//let trainPath = @"../data/train.csv"
//[<Literal>]
//let testPath = @"../data/test.csv"
//[<Literal>]
//let attributesPath = @"../data/attributes.csv"
[<Literal>]
let productsPath =  @"..\cache\product_descriptions.csv"

[<Literal>]
let submissionPath =  @"../data/"

//type Train = CsvProvider<trainPath,Schema=",,,,float">
//type Test = CsvProvider<testPath>
type Descriptions = CsvProvider<productsPath>

let sample = seq {
//    yield! Train.GetSample().Rows |> Seq.map (fun x -> x.Product_title)
//    yield! Test.GetSample().Rows |> Seq.map (fun x -> x.Product_title)
        yield! Descriptions.GetSample().Rows |> Seq.map (fun r -> r.Product_description)
    }

#load "Utilities.fs"
open HomeDepot.Utilities
open FSharp.Collections.ParallelSeq

let titles = 
    sample
//    |> Seq.distinct
    |> PSeq.map preprocess
//    |> Seq.toArray

let path = @"C:\users\Taylor\desktop\Kaggle\descriptions.txt"
let file = System.IO.File.WriteAllLines(path, titles)

#r @"C:\Users\Taylor\Documents\Visual Studio 2015\Projects\Word2Vec\Word2Vec.Net\bin\Debug\Word2Vec.Net.dll"
open Word2Vec

let output = @"C:\users\Taylor\desktop\Kaggle\output.txt"
let vocab = @"C:\users\Taylor\desktop\Kaggle\vocab.txt"
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
distance.Search("for")

let analogy = Word2Vec.Net.WordAnalogy(output)
analogy.Search("chrome steel plastic")