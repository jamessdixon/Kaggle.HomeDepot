
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

trainData.Rows |> Seq.length
testData.Rows |> Seq.length


