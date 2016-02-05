(*
Simple example:
1. create a learner that 'learns' from
examples, products and attributes and returns a
predictor function, 
2. evaluate the predictor RMSE
The model here is simply 'predict the average'
*)

#I @"../packages/"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"

#load "Core.fs"
open HomeDepot.Core

let learn : Learn = function (examples,products,attributes) ->
    
    let average = 
        examples 
        |> Seq.averageBy (fun ex -> ex.Relevance)
        |> float
    
    let model (observation:Observation) = average
    
    model

evaluate learn
|> printfn "RMSE: %.4f"