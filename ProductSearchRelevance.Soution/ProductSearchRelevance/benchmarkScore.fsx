
#r "../packages/Accord.3.0.2/lib/net40/Accord.dll"
#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"
#r "../packages/Accord.Math.3.0.2/lib/net40/Accord.Math.dll"
#r "../packages/Accord.Statistics.3.0.2/lib/net40/Accord.Statistics.dll"

open FSharp.Data
open Accord.Math
open Accord.Statistics
open Accord.Statistics.Links
open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Fitting

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

let productDescription uid = 
    let pd = 
        products.Rows 
        |> Seq.tryFind(fun pd -> pd.Product_uid = uid)
    if pd.IsSome then
        Some pd.Value.Product_description
    else None


//WordMatch needs to have a fuzzy match
//The sample R script uses a regex like this:
//pattern <- paste("(^| )",words[i],"($| )",sep="")
//I assume we can dup using .NET Reg Ex class like this:
//let pattern = @"\b(\w+)\s\1\b";
//let rgx = new Regex(pattern, RegexOptions.IgnoreCase);
//let matches = rgx.Matches(input)
//Until then, it does an exact match

let wordMatch (words:string) title (desc:option<string>) =
    let words' = words.Split(' ')
    let uniqueWords = words' |> Seq.distinct
    let numberOfWords = uniqueWords |> Seq.length
    let numberInTitle =  uniqueWords |> Seq.filter(fun w -> w = title) |> Seq.length
    let numberInDescription =
        if desc.IsNone then 0
        else uniqueWords |> Seq.filter(fun w -> w = desc.Value) |> Seq.length
    numberInTitle,numberInDescription,numberOfWords

let trainInput = 
    train.Rows 
    |> Seq.map(fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> Seq.map(fun (t,d,w) -> [|(float)t;(float)d;(float)w|])
    |> Seq.toArray

let trainOutput = 
    train.Rows
    |> Seq.map(fun t -> (float)t.Relevance)
    |> Seq.toArray

let testInput = 
    test.Rows 
    |> Seq.map(fun w -> wordMatch w.Search_term w.Product_title (productDescription w.Product_uid))
    |> Seq.map(fun (t,d,w) -> [|(float)t;(float)d;(float)w|])
    |> Seq.toArray

let regression = new GeneralizedLinearRegression(new ProbitLinkFunction(), 3)
let teacher = new IterativeReweightedLeastSquares(regression)

let rec runTeacher delta =
    let newDelta = teacher.Run(trainInput,trainOutput)
    if delta > 0.001 then
        runTeacher newDelta

runTeacher 0.0

//The results are wrong, and I am not sure why
//Need to look at Accord's glm closer

let testOutput = regression.Compute(testInput)
let submission = 
    Seq.zip test.Rows testOutput
    |> Seq.map(fun (r,o) -> r.Id, o)




