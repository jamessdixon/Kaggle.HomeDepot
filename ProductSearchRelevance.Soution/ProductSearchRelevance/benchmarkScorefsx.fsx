
#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"
open FSharp.Data

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

#time

//let wordMatch (words:string) title desc =
//    let words' = words.Split(' ')
//    words' Seq.map(fun w -> )

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
//
//cat("Get number of words and word matching title in train\n")
//train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
//train$nmatch_title <- train_words[,1]
//train$nwords <- train_words[,2]
//train$nmatch_desc <- train_words[,3]
//
//cat("Get number of words and word matching title in test\n")
//test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
//test$nmatch_title <- test_words[,1]
//test$nwords <- test_words[,2]
//test$nmatch_desc <- test_words[,3]
//
//rm(train_words,test_words)
//
//cat("A simple linear model on number of words and number of words that match\n")
//glm_model <- glm(relevance~nmatch_title+nmatch_desc+nwords,data=train)
//test_relevance <- predict(glm_model,test)
//test_relevance <- ifelse(test_relevance>3,3,test_relevance)
//test_relevance <- ifelse(test_relevance<1,1,test_relevance)
//
//submission <- data.frame(id=test$id,relevance=test_relevance)
//write_csv(submission,"benchmark_submission.csv")
//print(Sys.time()-t)
