#load "Dependencies.fsx"

open HomeDepot.Model
open HomeDepot.Features

#r "alglibnet2/lib/alglibnet2.dll"

let features = 
    [| 
        ``Taylor / unique words in search terms``   // 0.528351
        ``Taylor / search terms length``            // 0.525547
        ``Taylor / product title length``           // 0.532044 
        ``Taylor / product description length``     // 0.524044 
        ``Taylor / attributes not in description``  // 0.513271 

        ``Taylor / search terms in title``
        ``Taylor / search terms in description``
        ``Taylor / search terms in attributes``     // 0.481416
//        ``Taylor / search terms matched``

        ``Taylor / matching last search term and last title word``

        ``number of attributes``

        ``Taylor / seq matching search terms and title terms``
        ``Taylor / rev seq matching search terms and title terms``

        ``Taylor / search terms vs title position score`` // 469982

        ``Taylor / % search terms in description``
        ``Taylor / % search terms in title``

        ``Taylor / product has attributes``
        ``Taylor / attribute names found in search terms``

        ``Taylor / brand match`` // 0.468703

        ``number of non-bullet attributes``

        ``single word search``
        ``2 - 5 words search``
        ``10 words or more search`` // 0.467981

//        ``product with no brand``
//
//        ``brand matches search terms``
//
//        ``matching voltage``
//        ``matching wattage``
//        ``matching amperage``
//        ``matching gallons``
//        ``matching pounds``
    |]


let learner (sample:Example[]) =
   
    let features = 
        featurizer features (sample |> Array.map snd)

    printfn "Preparing data"

    let trainInputOutput =
        sample
        |> Array.Parallel.map (fun (label,observation) ->
            let fs = observation |> extract features
            Array.append fs [| label |])
        |> array2D

    printfn "Training random forest"

    let trees = 600
    let treeTrainSize = 0.1
    let sampleSize = sample.Length
    let featureCount = features.Length

    let _info, forest, forestReport =
        alglib.dfbuildrandomdecisionforest(trainInputOutput, sampleSize, featureCount, 1, trees, treeTrainSize)
    
    printfn "RDF RMS Error: %f; Out-of-bag RMS Error: %f" forestReport.rmserror forestReport.oobrmserror

    let predictor (obs:Observation) = 
        let fs = obs |> extract features
        let mutable result : float [] = [||]
        alglib.dfprocess(forest, fs, &result)
        result.[0]

    predictor

//evaluate 10 learner

let test = learner trainset

createSubmission learner
