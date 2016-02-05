namespace HomeDepot

open CsvData

module Core =

    type Quality = float
    type Example = Sample * Quality

    type Predictor = Sample -> Quality
    
    type AttributeMap = Map<int, seq<int * string * string>>

    (*
    Crude validation
    *)

    // https://www.kaggle.com/wiki/RootMeanSquaredError
    let rmse (predictor:Predictor) (examples:Example seq) =
        examples
        |> Seq.averageBy (fun (obs,qual) ->
            let delta = predictor obs - qual
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
        let size = trainSamples.Length
        let sampleSize = size * 3 / 4
        let trainingSamples = scoredTrainSamples |> Array.take sampleSize
        let validationSamples = scoredTrainSamples |> Array.skip sampleSize
        
        // get a trained model for prediction
        let attribMap = getAttributeMap()
        let predictor = learn trainingSamples attribMap

        // calculate RMSE for validation sample predictions
        rmse predictor validationSamples
