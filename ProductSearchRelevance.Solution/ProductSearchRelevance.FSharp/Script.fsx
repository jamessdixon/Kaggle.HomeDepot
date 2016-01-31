
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

//train
//id
//productTitle
//productUid
//relivance
//serach phrase

//test
//id
//productTitle
//productUid
//serach phrase

//Submit
//id
//relivance

//descriptions
//productUid
//productDescription

//attribututes
//productUid
//Name
//Value

trainData.Rows |> Seq.length
testData.Rows |> Seq.length

//beanchmark script
//SkLearn one -> beats bench markett (random forest)

//NLP tools like "tokenization" (splitting a text into words) and "stemming" (taking the stem of a work, ie. "model", "models", "modeling" -> "model").
//Then it feed the feature matrix to train random forest regression model. It also wraps the model in a "bagging" regressor to improve the results a bit. That's it, this gives you a score of ~0.5

//Attributes are intended to be used. Relevance instructions were instructions given to raters when the data was prepared. Raters were only give product title and description.
//The raters were given search term, product title, product image and product description. 







