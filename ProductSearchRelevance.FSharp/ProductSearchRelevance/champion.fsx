#load "Dependencies.fsx"

open HomeDepot.Model
open HomeDepot.Features

#r "alglibnet2/lib/alglibnet2.dll"

let features = 
    [| 
        ``Unique search terms matched in title``        
        ``% unique search terms matched in title``          // 0.495331 
        ``Unique search terms matched in description``      // 0.490977
        ``% unique search terms matched in description``    // 0.490608
        ``Unique search terms``                             // 0.489939
        ``Duplicate search terms``                          // 0.489816
        ``Search terms length``                             // 0.487736
        ``Brand match in search terms``                     // 0.487680
        ``First search terms and title words match``        // 0.487031
        ``Last search terms and first title words match``   // 0.486734
        ``Position of search terms in title``               // 0.487033
        ``Reverse position of search terms in title``       // 0.485098
//        ``Unmatched search terms in title``
        ``Longest matching seq between search terms and title``     // 0.484862
        ``Longest backwards matching seq between search terms and title`` // 0.484120
        ``Number of non-bullet attributes``                 // 0.478353
//        ``Number of bullet attributes``                     // 0.484141 !?
//        ``Number of attributes``                            // 0.478329
        // re-added duplicate search terms                  // 0.478259
        ``Search terms contain number``                     // 0.474616 after fixing attributes bug
        ``Close product weight``                            // 0.474410
        ``Has weight``                                      // 0.473804
        ``Close product length``
        ``Has length``                                      // 0.473695
        ``Number of matching attribute names``              // 0.470940
        ``Last search terms and title words match``         // 0.470988, !?
        ``Search terms specificity``                        // 0.465452 > Kaggle 0.48527
        ``Specificity weighted Search terms match``         // 0.463280
        ``Frequency weighted title match``                  // 0.461863 > Kaggle 0.47902
        ``Contains a surface``                              // 0.461150
        ``Brand mismatch``                                  // 0.461061
        ``Title contains last search term``                 // 0.455789 - with modified aggressive pre-proc
        ``Product type match``                              // 0.455558
        ``Bigrams title match``
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
    let proportionHeldOut = 0.1
    let sampleSize = sample.Length
    let featureCount = features.Length
    let featuresUsed = sqrt (float featureCount) |> ceil |> int

    let _info, forest, forestReport =
        alglib.dfbuildrandomdecisionforestx1(
            trainInputOutput, 
            sampleSize, 
            featureCount, 
            1, 
            trees, 
            featuresUsed, 
            proportionHeldOut)
    
    printfn "Out-of-bag RMS err: %f, avg err: %f" forestReport.oobrmserror forestReport.oobavgerror

    let predictor (obs:Observation) = 
        let fs = obs |> extract features
        let mutable result : float [] = [||]
        alglib.dfprocess(forest, fs, &result)
        result.[0]

    predictor

//evaluate 10 learner

let test = learner trainset

//createSubmission learner
