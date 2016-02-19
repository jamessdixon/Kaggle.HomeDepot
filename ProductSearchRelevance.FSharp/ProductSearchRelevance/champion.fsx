#I @"../packages/"

#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#load "Model.fs"
open HomeDepot.Model

#r @"StemmersNet\lib\net20\StemmersNet.dll"
#r @"FParsec/lib/net40-client/FParsecCS.dll"
#r @"FParsec/lib/net40-client/FParsec.dll"
#load "Features.fs"
open HomeDepot.Features

// TODO fix
let standardizeMeasures = id

let ``unique words in search terms`` : FeatureLearner =
    fun sample ->
        fun obs ->
            obs.SearchTerm 
            |> standardizeMeasures
            |> splitBy ' ' 
            |> Array.distinct 
            |> Array.length 
            |> float

let ``words in product title`` : FeatureLearner =
    fun sample ->
        fun obs ->
            obs.Product.Title 
            |> standardizeMeasures
            |> splitBy ' ' 
            |> Array.length 
            |> float

let ``title length`` : FeatureLearner =
    fun sample ->
        fun obs ->
            obs.Product.Title.Length
            |> float

let ``description length`` : FeatureLearner =
    fun sample ->
        fun obs ->
            obs.Product.Description.Length
            |> float

let ``forward title match length`` : FeatureLearner =
    fun sample ->
        fun obs ->
            let search =
                obs.SearchTerm 
                |> standardizeMeasures
                |> splitBy ' '
            let title = 
                obs.Product.Title
                |> standardizeMeasures
                |> splitBy ' ' 
            (search,title) ||> Seq.zip |> Seq.takeWhile (fun (a,b) -> isMatch a b) |> Seq.length |> float

let ``backwards title match length`` : FeatureLearner =
    fun sample ->
        fun obs ->
            let search =
                obs.SearchTerm 
                |> standardizeMeasures
                |> splitBy ' ' 
                |> Array.rev
            let title = 
                obs.Product.Title
                |> standardizeMeasures
                |> splitBy ' ' 
                |> Array.rev
            (search,title) ||> Seq.zip |> Seq.takeWhile (fun (a,b) -> isMatch a b) |> Seq.length |> float

let ``query title position score`` : FeatureLearner =
    fun sample ->
        fun obs ->
            let search =
                obs.SearchTerm 
                |> standardizeMeasures
                |> fun x -> x.ToLowerInvariant ()
                |> splitBy ' ' 
            let title = 
                obs.Product.Title
                |> standardizeMeasures
                |> fun x -> x.ToLowerInvariant ()
                |> splitBy ' ' 
                |> Array.rev
            let titleLength = float title.Length
            search 
            |> Array.map (fun w -> 
                match (title |> Array.tryFindIndex (isMatch w)) with
                | None -> 0.
                | Some(i) -> float i / titleLength)
            |> Array.average

let ``search and title last words match`` : FeatureLearner =
    fun sample ->
        fun obs ->
            let search =
                obs.SearchTerm 
                |> splitBy ' ' 
            let title = 
                obs.Product.Title
                |> splitBy ' ' 
            if isMatch search.[search.Length-1] title.[title.Length-1] then 1. else 0.


(*
9639
20954
0
0
29330
30463
29791
0
0
0
0
2
2
0
19
8
4
1
11
168
0
3
1
*)

#r "alglibnet2/lib/alglibnet2.dll"

let features = 
    [| 
        ``unique words in search terms``
        ``words in product title``
        
        ``title length``
        ``description length``

        ``forward title match length``
        ``backwards title match length``

        ``query title position score``

        ``search and title last words match``

        ``number of attributes``
        ``number of non-bullet attributes``
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
        ``product with no brand``

        ``matching voltage``
        ``matching wattage``
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

evaluate 10 learner

let test = learner trainset

createSubmission learner
