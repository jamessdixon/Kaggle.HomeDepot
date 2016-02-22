#load "Dependencies.fsx"

open HomeDepot.Model
open HomeDepot.Features

#r "Accord.Math/lib/net45/Accord.Math.dll"
#r "Accord/lib/net45/Accord.dll"
#r "Accord.Statistics/lib/net45/Accord.Statistics.dll"

open Accord.Statistics.Models.Regression.Fitting
open Accord.Statistics.Models.Regression

let features = 
    [| 
        ``number of attributes``
        ``no attributes``
        ``single word search``
        ``brand matches search terms``
        ``search terms and title % word intersection``
        ``% search terms in description``
        ``search terms in title, order weighted``
        ``search terms in title, reverse order weighted``
        ``words in title``
        ``words in description``
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
