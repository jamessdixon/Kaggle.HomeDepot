
#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"
#r "../packages/Accord.Statistics.3.0.2/lib/net40/Accord.Statistics.dll"

open FSharp.Data
open Accord.Statistics
open Accord.Statistics.Links
open Accord.Statistics.Models.Regression
open Accord.Statistics.Models.Regression.Fitting

[<Literal>]
let trainDataPath = "../data/train.csv"
type TrainData = CsvProvider<trainDataPath>
let trainData = TrainData.GetSample()

[<Literal>]
let testDataPath = "../data/test.csv"
type TestData = CsvProvider<testDataPath>
let testData = TestData.GetSample()

[<Literal>]
let productDescriptionsPath = "../data/product_descriptions.csv"
type ProductDescriptions = CsvProvider<productDescriptionsPath>
let productDescriptions = ProductDescriptions.GetSample()

let getProductDescription uid = 
    let pd = productDescriptions.Rows 
             |> Seq.tryFind(fun pd -> pd.Product_uid = uid)
    if pd.IsSome then
        Some pd.Value.Product_description
    else None

type Train = {id:int;title:string;uid:int; relivance:float;phrase:string;description:option<string>}
type Test = {id:int;title:string;uid:int;phrase:string;description:option<string>}

let train = 
    trainData.Rows 
    |> Seq.map(fun r -> {id=r.Id;title=r.Product_title;
                        uid=r.Product_uid;
                        relivance=(float)r.Relevance;
                        phrase=r.Search_term;
                        description = getProductDescription r.Product_uid})
    |> Seq.toArray


let test =
    testData.Rows
    |> Seq.map(fun r -> {id=r.Id;title=r.Product_title;
                        uid=r.Product_uid;
                        phrase=r.Search_term;
                        description = getProductDescription r.Product_uid})
    |> Seq.toArray

let wordMatch (words:string) title (desc:option<string>) =
    let words' = words.Split(' ')
    let uniqueWords = words' |> Seq.distinct
    let numberOfWords = uniqueWords |> Seq.length
    let numberInTitle =  uniqueWords |> Seq.filter(fun w -> w = title) |> Seq.length
    let numberInDescription =
        if desc.IsNone then 0
        else uniqueWords |> Seq.filter(fun w -> w = desc.Value) |> Seq.length
    numberInTitle,numberOfWords,numberInDescription

let trainInput = 
    train 
    |> Seq.map(fun w -> wordMatch w.phrase w.title w.description)
    |> Seq.map(fun (t,w,d) -> [|(float)t;(float)w;(float)d|])
    |> Seq.toArray

let trainOutput = 
    train
    |> Seq.map(fun t -> t.relivance)
    |> Seq.toArray

let testInput = 
    test 
    |> Seq.map(fun w -> wordMatch w.phrase w.title w.description)
    |> Seq.map(fun (t,w,d) -> [|(float)t;(float)w;(float)d|])
    |> Seq.toArray

//glm_model <- glm(relevance~nmatch_title+nmatch_desc+nwords,data=train)
//test_relevance <- predict(glm_model,test)
//test_relevance <- ifelse(test_relevance>3,3,test_relevance)
//test_relevance <- ifelse(test_relevance<1,1,test_relevance)
let regression = new GeneralizedLinearRegression(new ProbitLinkFunction(), 3)
let teacher = new IterativeReweightedLeastSquares(regression)

let rec runTeacher delta =
    let newDelta = teacher.Run(trainInput,trainOutput)
    if delta > 0.001 then
        runTeacher newDelta

runTeacher 0.0

//submission <- data.frame(id=test$id,relevance=test_relevance)
//write_csv(submission,"benchmark_submission.csv")
//print(Sys.time()-t)
