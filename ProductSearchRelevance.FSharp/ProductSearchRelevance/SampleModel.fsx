(*
Simple example:
1. create a learner that 'learns' from
examples, products and attributes and returns a
predictor function, 
2. evaluate the predictor RMSE
The model here is simply 'predict the average'
*)

#load "Dependencies.fsx"
open HomeDepot.Model

let learner sample = 
    
    let average = 
        sample 
        |> Seq.averageBy (fun (label,observation) -> label)
    
    let model (observation:Observation) = average
    
    model

evaluate 10 learner

let test = learner trainset
rmse (trainset |> Array.map (fun (x,y) -> x, test y))