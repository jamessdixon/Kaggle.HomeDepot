namespace HomeDepot

open CsvData

module Core =
    open System

    type Quality = float
    type Example = Sample * Quality

    type Predictor = Sample array -> Quality array
    
    type AttributeMap = Map<int, seq<int * string * string>>

    (*
    Crude validation
    *)

    // https://www.kaggle.com/wiki/RootMeanSquaredError
    let rmse (actual:Quality seq) (expected:Quality seq) =
        Seq.zip actual expected
        |> Seq.averageBy (fun (act,exp) ->
            let delta = act - exp
            delta * delta)
        |> sqrt

    type Learn = Example array -> AttributeMap -> Predictor

    // crude evaluation: learn on 3/4 of the sample,
    // compute the RMSE on the last 1/4.
    // TODO: allow only learning on the products and
    // attributes covered by the training sample
    // TODO: k-fold
    let evaluate (learn:Learn) =

        let trainSamples = getTrainSamples()
        let trainOutput = getTrainOutput()

        // partition training data
        let scoredTrainSamples = Array.zip trainSamples trainOutput
        let rng = System.Random(4231982)
        let trainingSamples, validationSamples = scoredTrainSamples |> Array.partition (fun _ -> rng.NextDouble() <= 0.75)
//        let size = trainSamples.Length
//        let sampleSize = size * 3 / 4
//        let trainingSamples = scoredTrainSamples |> Array.take sampleSize
//        let validationSamples = scoredTrainSamples |> Array.skip sampleSize
        printfn "%d training samples, %d validation samples" trainingSamples.Length validationSamples.Length
        
        // get a trained model for prediction
        let attribMap = getAttributeMap()
        let predictor = learn trainingSamples attribMap

        // calculate RMSE for validation sample predictions
        let predictions = predictor (validationSamples |> Array.map fst)
        rmse predictions (validationSamples |> Seq.map snd)

    let submission (learn:Learn) =
        // get a trained model for prediction
        let trainSamples = getTrainSamples()
        let trainOutput = getTrainOutput()
        let trainingSamples = Array.zip trainSamples trainOutput
        let attribMap = getAttributeMap()
        let predictor = learn trainingSamples attribMap
        
        // make predicitions on test samples
        let testSamples = getTestSamples()
        let predictions = predictor testSamples

        // format output
        let submission =
            Seq.zip testSamples predictions
            |> Seq.map (fun (s,p) -> sprintf "%d,%A" s.Id p)
            |> List.ofSeq

        let writeResults name rows =
            let outputPath = __SOURCE_DIRECTORY__ + sprintf "../../data/%s_submission_FSharp.csv" name
            IO.File.WriteAllLines(outputPath, "id,relevance" :: rows)
        writeResults (DateTime.UtcNow.ToString("yyyy-MM-dd_HHmm")) submission