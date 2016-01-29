
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

//word_match <- function(words,title,desc){
//  n_title <- 0
//  n_desc <- 0
//  words <- unlist(strsplit(words," "))
//  nwords <- length(words)
//  for(i in 1:length(words)){
//    pattern <- paste("(^| )",words[i],"($| )",sep="")
//    n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
//    n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
//  }
//  return(c(n_title,nwords,n_desc))
//}
let wordMatch (words:string) title (desc:option<string>) =
    let words' = words.Split(' ')
    let uniqueWords = words' |> Seq.distinct
    let numberOfWords = uniqueWords |> Seq.length
    let pattern = uniqueWords |> Seq.map(fun w -> "(^| )" + w + "($| )")
    let numberInTitle =  pattern |> Seq.filter(fun w -> w = title) |> Seq.length
    let numberInDescription =
        if desc.IsNone then 0
        else pattern |> Seq.filter(fun w -> w = desc.Value) |> Seq.length
    numberInTitle,numberOfWords,numberInDescription


//train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
//train$nmatch_title <- train_words[,1]
//train$nwords <- train_words[,2]
//train$nmatch_desc <- train_words[,3]
//test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
//test$nmatch_title <- test_words[,1]
//test$nwords <- test_words[,2]
//test$nmatch_desc <- test_words[,3]
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
