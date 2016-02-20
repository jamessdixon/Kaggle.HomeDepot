#I @"../packages/"
#r "Accord/lib/net40/Accord.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#r "Accord.Math/lib/net40/Accord.Math.dll"
#r "Accord.Statistics/lib/net40/Accord.Statistics.dll"
#r "Accord.MachineLearning/lib/net40/Accord.MachineLearning.dll"

open System
open Accord
open FSharp.Data
open Accord.Math
open Accord.Statistics
open Accord.MachineLearning
open Accord.Statistics.Kernels
open Accord.MachineLearning.VectorMachines
open Accord.MachineLearning.VectorMachines.Learning

let data = [|[|-1.0;-1.0|];[|1.0;-1.0|];[|-1.0;1.0|];[|1.0;1.0|];
             [|-1.0;-1.0|];[|1.0;-1.0|];[|-1.0;-1.0|];[|1.0;1.0|];
             [|-1.0;-1.0|];[|1.0;-1.0|];[|-1.0;1.0|];[|1.0;1.0|];
             [|-1.0;-1.0|];[|1.0;-1.0|];[|-1.0;1.0|];[|1.0;1.0|]|]

let xor = [|-1;1;1;-1;-1;1;1;-1;-1;1;1;-1;-1;1;1;-1|]

let size = data |> Seq.length
let folds = 10
let crossValidation = new CrossValidation(size, folds)

let fitting k (indicesTrain:int[]) (indicesValidation:int[]) = 
    let svm = new KernelSupportVectorMachine(new Polynomial(2), 2)
    let trainingInputs = data.Submatrix(indicesTrain);
    let trainingOutputs = xor.Submatrix(indicesTrain);
    let validationInputs = data.Submatrix(indicesValidation);
    let validationOutputs = xor.Submatrix(indicesValidation);

    let smo = new SequentialMinimalOptimization(svm, trainingInputs, trainingOutputs)
    let trainingError = smo.Run()
    let validationError = smo.ComputeError(validationInputs, validationOutputs)
    new CrossValidationValues<System.Object>(svm, trainingError, validationError)

let fittingFunction = new CrossValidationFittingFunction<System.Object>(fitting)

crossValidation.Fitting <- fittingFunction
let result = crossValidation.Compute();

let trainingErrors = result.Training.Mean;
let validationErrors = result.Validation.Mean;



//#I @"../packages/"
//#r "FSharp.Data/lib/net40/FSharp.Data.dll"
//#r "Accord/lib/net40/Accord.dll"
//#r "Accord.Math/lib/net40/Accord.Math.dll"
//#r "Accord.Statistics/lib/net40/Accord.Statistics.dll"
//#r "Accord.MachineLearning/lib/net40/Accord.MachineLearning.dll"
//
//open System
//open Accord
//open FSharp.Data
//open Accord.Math
//open Accord.Statistics
//open Accord.MachineLearning
//open Accord.Statistics.Kernels
//open Accord.MachineLearning.VectorMachines
//open Accord.MachineLearning.VectorMachines.Learning
//
//[<Literal>]
//let uri = "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
//type Iris = CsvProvider<uri>
//
//let iris = Iris.GetSample().Rows |> Seq.toArray
//Tools.Shuffle(iris) |> ignore
//let irisLength = iris |> Seq.length |> float
//let attachmentPoint = (irisLength * 0.8) |> int
//
//let dataset = iris |> Seq.take attachmentPoint
//let validation = iris |> Seq.skip attachmentPoint  
//
//let flowerCode flowerDescripion =
//    match flowerDescripion with
//    | "Iris Setosa" -> 0
//    | "Iris Versicolour" -> 1
//    | "Iris Virginica" -> 2
//    | _ -> -1
//
//let flowerDescripion flowerCode =
//    match flowerCode with
//    | 0 -> "Iris Setosa"
//    | 1 -> "Iris Versicolour"
//    | 2 -> "Iris Virginica"
//    | _ -> "Fault"
//
////http://accord-framework.net/docs/html/T_Accord_MachineLearning_CrossValidation.htm
//let size = dataset |> Seq.length
//let folds = 10
//let crossValidation = new CrossValidation(size, folds)
//
//let fitting (k:int) (indicesTrain:int[]) (indicesValidation:int[])  = 
//    let svm = new KernelSupportVectorMachine(new Polynomial(4), 4)
//    let trainingInputs = 
//        dataset 
//        |> Seq.map(fun i -> [|float i.``0.2``; float i.``1.4``; float i.``3.5``; float i.``5.1``|]) 
//        |> Seq.toArray
//    let trainingInputs' = Matrix.Submatrix<_>(trainingInputs,indicesTrain)
//    let trainingOutputs =
//        dataset 
//        |> Seq.map(fun i -> flowerCode i.``Iris-setosa``) 
//        |> Seq.toArray
//    let trainingOutputs' = Matrix.Submatrix<_>(trainingOutputs,indicesTrain)
//    let smo = new SequentialMinimalOptimization(svm, trainingInputs', trainingOutputs')
//    let trainingError = smo.Run()
//    let validationInputs = 
//        validation 
//        |> Seq.map(fun i -> [|float i.``0.2``; float i.``1.4``; float i.``3.5``; float i.``5.1``|]) 
//        |> Seq.toArray
//    let validationInputs' = Matrix.Submatrix<_>(validationInputs,indicesValidation)
//    let validationOutputs =
//        validation 
//        |> Seq.map(fun i -> flowerCode i.``Iris-setosa``) 
//        |> Seq.toArray
//    let validationOutputs' = Matrix.Submatrix<_>(validationOutputs,indicesValidation)
//    let validationError = smo.ComputeError(validationInputs', validationOutputs')
//    new CrossValidationValues<System.Object>(svm, trainingError, validationError)
//
//let fittingFunction = new CrossValidationFittingFunction<System.Object>(fitting)
//
//crossValidation.Fitting <- fittingFunction
//let result = crossValidation.Compute();
//
//let trainingErrors = result.Training.Mean;
//let validationErrors = result.Validation.Mean;
//
