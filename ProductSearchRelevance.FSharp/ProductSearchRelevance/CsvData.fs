namespace HomeDepot

[<RequireQualifiedAccess>]
module CsvData =

    type Sample = 
      { Id : int
        ProductId : int
        Title : string
        Description : string
        Query : string }

    open FSharp.Data

    type Train = CsvProvider< "../data/train.csv" >
    type Products = CsvProvider< "../data/product_descriptions.csv" >
    type Attributes = CsvProvider< "../data/attributes.csv" >
    type Test = CsvProvider< "../data/test.csv" >

    let attributes = Attributes.GetSample()

    let getAttributeMap() = 
      printfn "Building Product Attribute map..."
      attributes.Rows
      |> Seq.map (fun r -> r.Product_uid, r.Name, r.Value)
      |> Seq.groupBy (fun (i, _, _) -> i)
      |> Map.ofSeq

    let products = Products.GetSample()

    let productDescMap = 
      lazy (printfn "Building Product Description map..."
            products.Rows
            |> Seq.map (fun pd -> pd.Product_uid, pd.Product_description)
            |> Map.ofSeq)

    let inline productDescription uid = productDescMap.Value |> Map.find uid

    let sample id productId title query = 
      { Id = id
        ProductId = productId
        Title = title
        Description = productDescription productId
        Query = query }

    let train = Train.GetSample()

    let getTrainSamples() = 
      train.Rows
      |> Seq.map (fun r -> sample r.Id r.Product_uid r.Product_title r.Search_term)
      |> Array.ofSeq

    let getTrainOutput() = 
      train.Rows
      |> Seq.map (fun t -> float t.Relevance)
      |> Array.ofSeq

    let test = Test.GetSample()

    let getTestSamples() = 
      test.Rows
      |> Seq.map (fun r -> sample r.Id r.Product_uid r.Product_title r.Search_term)
      |> Array.ofSeq
