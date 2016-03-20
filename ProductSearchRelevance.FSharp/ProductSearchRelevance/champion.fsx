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

let model = learner trainset

//createSubmission learner

(*
// worst mistakes

trainset
|> Array.map (fun (l,o) -> l, model o, o.SearchTerm, o.Product.Title)
|> Seq.sortByDescending (fun (a,b,c,d) -> abs (a-b))
|> Seq.take 100
|> Seq.toArray

*)

(*
// create file with extracted features

let createFeaturesFile (sample:Example[]) =

    let features = 
        featurizer features (sample |> Array.map snd)

    printfn "Preparing data"

    let data = 
        sample
        |> Array.Parallel.map (fun (label,observation) ->
            let fs = observation |> extract features
            let row = Array.append [| label |] fs
            row 
            |> Array.map (sprintf "%.4f")
            |> String.concat ",")

    let desktop =
        System.Environment.SpecialFolder.Desktop
        |> System.Environment.GetFolderPath

    let path = System.IO.Path.Combine(desktop,"features.csv")

    System.IO.File.WriteAllLines(path,data)

createFeaturesFile trainset
*)

(*
// analysis of features importance
    
let featuresImportance () =

    let desktop =
        System.Environment.SpecialFolder.Desktop
        |> System.Environment.GetFolderPath

    let path = System.IO.Path.Combine(desktop,"features.csv")

    let labels, data =
        System.IO.File.ReadAllLines(path)
        |> Array.map (fun line -> 
            line.Split ',' 
            |> Array.map float 
            |> fun x -> x.[0], x.[1..])
        |> Array.unzip

    printfn "train"
    let model = 

        let trees = 600
        let proportionHeldOut = 0.1
        let sampleSize = data.Length
        let featureCount = data.[0].Length
        let featuresUsed = sqrt (float featureCount) |> ceil |> int

        let trainData = 
            Array.zip data labels
            |> Array.map (fun (fs,ls) -> Array.append fs [| ls |])
            |> array2D

        let _info, forest, forestReport =
            alglib.dfbuildrandomdecisionforestx1(
                trainData, 
                sampleSize, 
                featureCount, 
                1, 
                trees, 
                featuresUsed, 
                proportionHeldOut)

        let predictor (fs:float[]) = 
            let mutable result : float [] = [||]
            alglib.dfprocess(forest, fs, &result)
            result.[0]

        predictor

    let data = data.[..9999]
    let labels = labels.[..9999]
    let size = data.[0].Length
    let length = data.Length

    let rng = System.Random(123456)

    let perturb n i =
        let copy = data |> Array.map(Array.copy)
        for _ in 1 .. n do
            let orig = rng.Next(length)
            let dest = rng.Next(length)
            copy.[dest].[i] <- data.[orig].[i]
        copy

    let rmse sample =
        sample
        |> Seq.averageBy (fun (act, exp) ->
            let delta = act - exp
            delta * delta)
        |> sqrt

    printfn "test features"

    [ 
        for f in 0 .. (size - 1) ->
            printfn "  features %i" f

            let data' = perturb 5000 f
            let predictions = data' |> Array.map model
            
            let r = 
                Array.zip labels predictions
                |> rmse 
            f, r
    ]

*)