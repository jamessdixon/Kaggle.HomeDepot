
#r "../packages/Accord/lib/net40/Accord.dll"
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../packages/Accord.Math/lib/net40/Accord.Math.dll"
#r "../packages/Accord.Statistics/lib/net40/Accord.Statistics.dll"
#r "../packages/FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"
#r "../packages/AForge.Neuro/lib/AForge.Neuro.dll"
#r "../packages/Accord.Neuro/lib/net40/Accord.Neuro.dll"
#r "../packages/StemmersNet/lib/net20/StemmersNet.dll"
#r "../packages/FuzzyString/lib/FuzzyString.dll"

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open FSharp.Data
open Accord.Statistics.Models.Regression.Linear
open Iveonik.Stemmers
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent
open AForge.Neuro
open Accord.Neuro
open Accord.Neuro.Learning
open Accord.Statistics
open Accord

[<Literal>]
let trainDataPath = "../data/train.csv"
type Train = CsvProvider<trainDataPath>
let train = Train.GetSample()

[<Literal>]
let testDataPath = "../data/test.csv"
type Test = CsvProvider<testDataPath>
let test = Test.GetSample()

[<Literal>]
let productDescriptionsPath = "../data/product_descriptions.csv"
type Products = CsvProvider<productDescriptionsPath>
let products = Products.GetSample()

let productDescMap =
    products.Rows
    |> Seq.map (fun pd -> pd.Product_uid, pd.Product_description)
    |> Map.ofSeq

let productDescription uid = 
    productDescMap 
    |> Map.tryFind uid

let wordMatch (words:string) (title:string) (desc:string option) =
    let isMatch input word = 
        Regex.IsMatch(input, sprintf @"\b%s\b" (Regex.Escape word), RegexOptions.IgnoreCase)
    let words' = words.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let uniqueWords = words' |> Array.distinct
    let numberInTitle = uniqueWords |> Array.filter (isMatch title) |> Array.length
    let numberInDescription =
        match desc with
        | Some desc -> uniqueWords |> Array.filter (isMatch desc) |> Array.length
        | None -> 0
    numberInTitle, numberInDescription, uniqueWords.Length 

let trainInput = 
    train.Rows 
    |> Seq.map (fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> Seq.map (fun (t,d,w) -> [| float t; float d; float w |])
    |> Seq.toArray

train.Rows 
    |> Seq.countBy(fun t -> t.Relevance) 
    |> Seq.sortBy(fun t -> fst t)
    |> Seq.iter(fun t -> printfn "%A,%A" (fst t) (snd t))

printfn "Finished Prepping Data..."

let getClass relevance =
    match relevance with
    | 1.0 -> 0
    | 1.33 -> 1
    | 1.67 -> 2
    | 2.0 -> 3
    | 2.33 -> 4
    | 2.67 -> 5
    | 3.0 -> 6
    | _ -> 9

let getRelevance outputClass =
    match outputClass with
    | 0 -> 1.0
    | 1 -> 1.33
    | 2 -> 1.67
    | 3 -> 2.0
    | 4 -> 2.33
    | 5 -> 2.67
    | 6 -> 3.0
    | _ -> 2.0

let trainOutput = 
    train.Rows
    |> Seq.map (fun t -> float t.Relevance)
    |> Seq.map (fun r -> getClass r )
    |> Seq.toArray

let outputs = Accord.Statistics.Tools.Expand(trainOutput, -1.0, 1.0)
let activationFunction = new BipolarSigmoidFunction()
let activationNetwork = ActivationNetwork(activationFunction,3, 5, 8) 
let randomizer = new NguyenWidrow(activationNetwork)
randomizer.Randomize() 
let teacher = new ParallelResilientBackpropagationLearning(activationNetwork)

//let mutable error = 1.0
//while error > 1e-5 do
//    error <- teacher.RunEpoch(trainInput, outputs)
//    ()

let mutable count = 1
while count < 60 do
    count <- count + 1
    teacher.RunEpoch(trainInput, outputs) |> ignore
    ()

printfn "Finished Training NN..."

let testInput =
    test.Rows 
    |> Seq.map (fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> Seq.map (fun (t,d,w) -> [| float t; float d; float w |])
    |> Seq.toArray

let testOutput =
    testInput 
    |> Array.map(fun r -> activationNetwork.Compute r)
    |> Array.map(fun a -> a |> Seq.mapi(fun i v -> i,v))
    |> Array.map(fun a -> a |> Seq.maxBy(fun (i,v) -> v))
    |> Array.map(fun o -> fst o)
    |> Array.map(fun v -> getRelevance v)

testOutput 
    |> Seq.countBy(fun t -> t) 
    |> Seq.sortBy(fun t -> fst t)
    |> Seq.iter(fun t -> printfn "%A,%A" (fst t) (snd t))

let testIds = test.Rows |> Seq.map(fun t -> t.Id)

printfn "Finished estiamting Test..."

let submission = 
    Seq.zip test.Rows testOutput
    |> Seq.map (fun (r,o) -> sprintf "%A,%A" r.Id o)
    |> Seq.toList

let outputPath = __SOURCE_DIRECTORY__ + @"../../data/benchmark_submission_FSharp.csv"
File.WriteAllLines(outputPath, "id,relevance" :: submission)

printfn "Finished outputing file..."


