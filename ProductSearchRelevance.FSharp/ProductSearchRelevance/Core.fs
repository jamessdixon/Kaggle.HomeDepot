namespace HomeDepot

module Core =

    type UID = int
    type Query = string
    type Observation = Query * UID

    type Quality = float
    type Example = Observation * Quality

    type Predictor = Observation -> Quality
    
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

    open FSharp.Data

    type Train = CsvProvider<"../data/train.csv">
    type TrainingExample = Train.Row

    type Products = CsvProvider<"../data/product_descriptions.csv">
    type Product = Products.Row

    type Attributes = CsvProvider<"../data/attributes.csv">
    type Attribute = Attributes.Row

    type Learn = (TrainingExample seq * Product seq * Attribute seq) -> Predictor

    let toExample (t:TrainingExample) = (t.Search_term, t.Product_uid), (float t.Relevance)

    // crude evaluation: learn on 3/4 of the sample,
    // compute the RMSE on the last 1/4.
    // TODO: allow only learning on the products and
    // attributes covered by the training sample
    // TODO: k-fold
    let evaluate (learn:Learn) =
        
        let training = Train.GetSample ()
        let products = Products.GetSample ()
        let attributes = Attributes.GetSample ()

        let size = training.Rows |> Seq.length

        let sampleSize = size * 3 / 4
        let trainingSample = training.Rows |> Seq.take sampleSize
        let validationSample = training.Rows |> Seq.skip sampleSize |> Seq.map toExample

        let model = learn (trainingSample, products.Rows, attributes.Rows)

        rmse model validationSample
