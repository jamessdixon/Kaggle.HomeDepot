#I @"../packages/"

#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#load "Model.fs"
open HomeDepot.Model

#r @"FParsec/lib/net40-client/FParsecCS.dll"
#r @"FParsec/lib/net40-client/FParsec.dll"
#r @"StemmersNet/lib/net20/StemmersNet.dll"
#load "Features.fs"
open HomeDepot.Features

#r "Accord.Math/lib/net45/Accord.Math.dll"
#r "Accord/lib/net45/Accord.dll"
#r "Accord.Statistics/lib/net45/Accord.Statistics.dll"

open Accord.Statistics.Models.Regression.Fitting
open Accord.Statistics.Models.Regression

let features = 
    [| 
        ``number of attributes``
        ``number of attributes log``
        ``number of attributes squared``
        ``no attributes``
        ``words in search terms``
        ``single word search``
        ``brand matches search terms``
        ``search terms and title % word intersection``
        ``% search terms in description``
        ``search terms in title, order weighted``
        ``search terms in title, reverse order weighted``
        ``words in title``
        ``words in description``
//        ``product with no brand``
//        ``matching voltage``
//        ``matching wattage``
    |]

let learner (sample:Example[]) =
    
    let normalize x = (x - 1.) / 2.
    let denormalize x = (2. * x) + 1.

    let output = sample |> Array.map (fst >> normalize)

    let features = 
        featurizer features (sample |> Array.map snd)

    let input = 
        sample 
        |> Array.map (snd >> extract features)

    let featuresCount = features.Length

    let logistic = LogisticRegression(featuresCount)
    let strategy = IterativeReweightedLeastSquares(logistic)

    let rec learn () =
        let error = strategy.Run(input,output)
        if error < 0.001
        then logistic
        else learn ()

    [ 0 .. featuresCount - 1]
    |> List.iter (fun i -> printfn " %b" (logistic.GetWaldTest(i).Significant))

    let predictor = learn ()

    extract features >> predictor.Compute >> denormalize

evaluate 10 learner

// createSubmission learner
