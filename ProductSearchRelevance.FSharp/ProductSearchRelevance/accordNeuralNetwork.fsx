
#I @"../packages/"
#r "Accord/lib/net40/Accord.dll"
#r "Accord.Math/lib/net40/Accord.Math.dll"
#r "AForge.Neuro/lib/AForge.Neuro.dll"
#r "Accord.Neuro/lib/net40/Accord.Neuro.dll"
#r "Accord.Statistics/lib/net40/Accord.Statistics.dll"

open System
open Accord
open FSharp.Data
open AForge.Neuro
open Accord.Neuro
open Accord.Statistics
open Accord.Neuro.Learning

let inputs = [|
     [| 0.0; 1.0; 1.0; 0.0 |]; // 0.0
     [| 0.0; 1.0; 0.0; 0.0 |]; // 0.0
     [| 0.0; 0.0; 1.0; 0.0 |]; // 0.0
     [| 0.0; 1.0; 1.0; 0.0 |]; // 0.0
     [| 0.0; 1.0; 0.0; 0.0 |]; // 0.0
     [| 1.0; 0.0; 0.0; 0.0 |]; // 1.0
     [| 1.0; 0.0; 0.0; 0.0 |]; // 1.0
     [| 1.0; 0.0; 0.0; 1.0 |]; // 1.0
     [| 0.0; 0.0; 0.0; 1.0 |]; // 1.0
     [| 0.0; 0.0; 0.0; 1.0 |]; // 1.0
     [| 1.0; 1.0; 1.0; 1.0 |]; // 2
     [| 1.0; 0.0; 1.0; 1.0 |]; // 2
     [| 1.0; 1.0; 0.0; 1.0 |]; // 2
     [| 0.0; 1.0; 1.0; 1.0 |]; // 2
     [| 1.0; 1.0; 1.0; 1.0 |]; // 2
     |]

let classes = [|
    0; 0; 0; 0; 0;
    1; 1; 1; 1; 1;
    2; 2; 2; 2; 2;
    |]

let outputs = Accord.Statistics.Tools.Expand(classes, -1.0, 1.0)
let activationFunction = new BipolarSigmoidFunction()
let activationNetwork = ActivationNetwork(activationFunction, 4, 5, 3) 
let randomizer = new NguyenWidrow(activationNetwork)
randomizer.Randomize() 
let teacher = new ParallelResilientBackpropagationLearning(activationNetwork)

let mutable error = 1.0
while error > 1e-5 do
    error <- teacher.RunEpoch(inputs, outputs)
    ()



