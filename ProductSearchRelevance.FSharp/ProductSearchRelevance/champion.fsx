#load "Dependencies.fsx"

open HomeDepot.Model
open HomeDepot.Features

#r "alglibnet2/lib/alglibnet2.dll"

let features = 
    [| 
        ``Unique search terms matched in title``        
        ``% unique search terms matched in title``           
        ``Unique search terms matched in description``
        ``% unique search terms matched in description``
        ``frequency weighted search terms matched in description``
        ``Unique search terms``
//        ``Duplicate search terms``                          
        ``Search terms length``                             
        ``Brand match in search terms``
        ``First search terms and title words match``
//        ``Last search terms and first title words match``
        ``Position of search terms in title``
        ``Reverse position of search terms in title``
//        ``Unmatched search terms in title``
        ``Longest matching seq between search terms and title``
        ``Longest backwards matching seq between search terms and title``
        ``Number of non-bullet attributes``
//        ``Number of bullet attributes``
//        ``Number of attributes``
        ``Search terms contain number``
//        ``Close product weight``
        ``Has weight``
//        ``Close product length``
        ``Has length``
        ``Number of matching attribute names``
        ``Last search terms and title words match``
        ``Search terms specificity``
        ``Specificity weighted Search terms match``
        ``Frequency weighted title match``
//        ``Contains a surface``
        ``Brand mismatch``
        ``Title contains last search term``
        ``Product type match``
        ``Bigrams title match``
//        ``Trigrams title match``
//        ``certifications and listings``
//        ``energy star certified``
//        ``material``
//        ``product length in``
//        ``product width in``
//        ``product height in``
//        ``product depth in``
        ``Word bigrams title match``
        ``Frequency weighted title match 2``
        ``Search vs Title similarity``
        ``Measure mismatch``
//        ``Dimension information``
    |]

let rflearner (sample:Example[]) =
   
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

//evaluate 10 rflearner

let rf = rflearner trainset

createSubmission rflearner
