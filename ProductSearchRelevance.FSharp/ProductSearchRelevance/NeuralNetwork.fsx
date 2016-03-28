#load "Dependencies.fsx"

open HomeDepot.Model
open HomeDepot.Features

#I @"../packages/"
#r @"Accord/lib/net45/Accord.dll"
#r @"Accord.MachineLearning\lib\net45\Accord.MachineLearning.dll"
#r @"Accord.Math\lib\net45\Accord.Math.dll"
#r @"Accord.Statistics\lib\net45\Accord.Statistics.dll"
#r @"Accord.Neuro/lib/net45/accord.neuro.dll"

open Accord
open Accord.Statistics
open AForge.Neuro
open Accord.Neuro
open Accord.Neuro.Learning

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
    |]

let nnlearner layer1 layer2 (sample:Example[]) =

    let features = 
        featurizer features (sample |> Array.map snd)

    printfn "Preparing data"

    let ys, xs =
        sample
        |> Array.Parallel.map (fun (label,observation) ->
            let l = label - 2.0
            let fs = observation |> extract features
            [| l |], fs)
        |> Array.unzip

    printfn "Training NN"

    let featuresCount = features.Length
    let activation = BipolarSigmoidFunction()
    let network = ActivationNetwork(activation, featuresCount, [| layer1; layer2; 1 |])
    NguyenWidrow(network).Randomize()

    let teacher = new ParallelResilientBackpropagationLearning(network)
    let epochs = 2000

    let rec learn iter =
        let error = teacher.RunEpoch(xs, ys)
        if (iter % 100 = 0) then printfn "%.3f / %i" error iter

        if error < 0.001 
        then 
            let predictor image = 
                network.Compute image 
                |> fun z -> z.[0] + 2.0
            predictor
        elif iter > epochs 
        then 
            let predictor image = 
                network.Compute image 
                |> fun z -> z.[0] + 2.0
            predictor
        else learn (iter + 1)

    let predictor = learn 0

    fun obs -> obs |> extract features |> predictor

let train = trainset.[..9999]
let test = trainset.[10000..19999]

let calibration = 
    [ for n1 in 4 .. 4 .. 20 do
        for n2 in 4 .. 4 .. 20 ->
            printfn "%i,%i" n1 n2
            let nn = nnlearner n1 n2 train
            (n1,n2), 
            test 
            |> Seq.map (fun (l,o) -> l, nn o)
            |> rmse
    ]
// evaluate 10 nnlearner

// let nn = nnlearner trainset

//createSubmission nnlearner

