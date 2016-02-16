#I @"../packages/"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"

#load "Model.fs"
open HomeDepot.Model

#r "Accord.Math/lib/net45/Accord.Math.dll"
#r "Accord/lib/net45/Accord.dll"
#r "Accord.Statistics/lib/net45/Accord.Statistics.dll"

open Accord.Statistics.Models.Regression.Fitting
open Accord.Statistics.Models.Regression

type Feature = Observation -> float
type NamedFeature = string * Feature

let extract features obs = 
    features |> Array.map (fun f -> f obs)

(* 
text manipulation 
*)

let lower (text:string) = text.ToLowerInvariant ()

let stopwords = set [ "and"; "or"; "the"; "a"; "an"; "of" ]
let removeStopwords (text:string Set) = Set.difference text stopwords

open System.Text.RegularExpressions

let matchWords = Regex(@"\w+",RegexOptions.Compiled)

let wordTokenizer (text:string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let learner (sample:Example[]) =
    
    let normalize x = (x - 1.) / 2.
    let denormalize x = (2. * x) + 1.

    let output = sample |> Array.map (fst >> normalize)

    let attributesCount : Feature = 
        fun obs -> obs.Product.Attributes.Count |> float
    
    let logAttributesCount : Feature = 
        fun obs -> obs.Product.Attributes.Count + 1 |> float |> log

    let sqAttributesCount : Feature = 
        fun obs -> obs.Product.Attributes.Count |> float |> fun x -> pown x 2

    let noAttributes : Feature =
        fun obs -> obs.Product.Attributes.Count |> fun x -> if x = 0 then 1. else 0.

    let wordsCount : Feature =
        fun obs -> wordTokenizer obs.SearchTerm |> Set.count |> float

    let singleWord : Feature = 
        fun obs -> 
            wordTokenizer obs.SearchTerm 
            |> Set.count 
            |> fun x -> if x = 1 then 1. else 0.

    let brand = "MFG Brand Name"

    let brandMatch : Feature =
        fun obs -> 
            match (obs.Product.Attributes.TryFind brand) with
            | None -> 0.
            | Some(brand) ->
                if obs.SearchTerm.ToLowerInvariant().Contains(brand.ToLowerInvariant())
                then 1.
                else -1.

    let titleMatch : Feature =
        fun obs -> 
            let terms = obs.SearchTerm |> lower |> wordTokenizer
            let desc = obs.Product.Title |> lower |> wordTokenizer
            let longest = max terms.Count desc.Count
            let intersect = Set.intersect terms desc |> Set.count
            float intersect / float longest

    let titleWords : Feature =
        fun obs -> 
            obs.Product.Title |> wordTokenizer |> Set.count |> float

    let descriptionWords : Feature =
        fun obs -> 
            obs.Product.Description |> wordTokenizer |> Set.count |> float

    let descriptionMatch : Feature =
        fun obs -> 
            let terms = obs.SearchTerm |> lower |> wordTokenizer
            let desc = obs.Product.Description |> lower
            let count = terms |> Set.fold (fun count word -> if desc.Contains word then count + 1 else count) 0
            float count / float terms.Count

    let positionMatch : Feature = 
        fun obs -> 
            let terms = 
                obs.SearchTerm 
                |> lower 
                |> fun x -> x.Split ' ' 
                |> Array.filter (fun x -> x <> "")
                |> Array.mapi (fun i word -> word, 1. / (pown 2. i))
            let desc = obs.Product.Title |> lower
            let score = 
                terms 
                |> Array.fold (fun acc (word,weight) -> 
                    if desc.Contains word then acc + weight else acc) 0.
            score

    let positionRevMatch : Feature = 
        fun obs -> 
            let terms = 
                obs.SearchTerm 
                |> lower 
                |> fun x -> x.Split ' ' 
                |> Array.filter (fun x -> x <> "")
                |> Array.rev
                |> Array.mapi (fun i word -> word, 1. / (pown 2. i))
            let desc = obs.Product.Title |> lower
            let score = 
                terms 
                |> Array.fold (fun acc (word,weight) -> 
                    if desc.Contains word then acc + weight else acc) 0.
            score

    let features = 
        [| 
            attributesCount
            logAttributesCount
            sqAttributesCount
            noAttributes
            wordsCount
            singleWord
            brandMatch
            titleMatch
            descriptionMatch
            positionMatch
            positionRevMatch
            titleWords
            descriptionWords
        |]
     

    let input = 
        sample 
        |> Array.map (snd >> extract features)

    let featuresCount = features.Length


    let logistic = LogisticRegression(featuresCount)
    let strategy = IterativeReweightedLeastSquares(logistic)

    let rec learn () =
        let error = strategy.Run(input,output)
        if error < 0.001
        then logistic
        else learn ()

    let predictor = learn ()

    extract features >> predictor.Compute >> denormalize

    (*
    // linear regression version, not significantly different

    let linear = Linear.MultipleLinearRegression(featuresCount,true)
    let result = linear.Regress(input,output,true)
    let predictor = linear
    let cutoff x = if x > 3. then 3. elif x < 1. then 1. else x
    extract features >> predictor.Compute >> denormalize >> cutoff
    *)

evaluate 10 learner

createSubmission learner
