namespace HomeDepot

module Features =

    open System
    open System.Collections.Concurrent
    open System.Text.RegularExpressions

    open HomeDepot.Utilities
    open HomeDepot.Model

    type Feature = Observation -> float

    type FeatureLearner = Observation [] -> Feature

    let featurizer (featureLearners:FeatureLearner[]) sample =
        featureLearners
        |> Array.map (fun learner -> learner sample)

    let extract (features:Feature[]) obs =
        features |> Array.map (fun f -> f obs)

    // measure how much time it takes to process 1,000 observations,
    // and extrapolate for the full trainset
    let speedTest (features:FeatureLearner[]) =

        let testSize = 1000
        let sample = trainset.[.. (testSize - 1)]

        let features = 
            featurizer features (sample |> Array.map snd)

        let stopwatch = System.Diagnostics.Stopwatch()

        features
        |> Array.iteri (fun i f ->
            stopwatch.Start()
            let fs = sample |> Array.map (fun (_,observation) -> f observation)
            let time = stopwatch.ElapsedMilliseconds
            let cost = float time * (float trainset.Length) / (float testSize) 
            printfn "%i: %i ms (%.0f ms)" i time cost
            stopwatch.Reset())

    (*
    Working with attribute
    *)

    let brandAttribute = "MFG Brand Name".ToLowerInvariant()

    (*
    Feature definitions
    *)

    let ``Taylor / unique words in search terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs.SearchTerm
                |> standardizeMeasures
                |> whiteSpaceTokenizer
                |> Array.distinct
                |> Array.length
                |> float

    let ``Taylor / search terms length`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs.SearchTerm
                |> standardizeMeasures
                |> String.length
                |> float

    let ``Taylor / product title length`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs.Product.Title
                |> standardizeMeasures
                |> String.length
                |> float

    open System.Text

    let foo (obs:Observation) =
        let description = obs.Product.Description
        obs.Product.Attributes 
        |> Seq.filter (fun kv -> kv.Key.StartsWith "bullet")
        |> Seq.fold (fun acc word -> acc + " " + word.Value) description
//        |> Seq.map (fun kv -> kv.Value)
//        |> Seq.fold (fun (state:StringBuilder) t -> state.Replace(t, t + " "))
//            (StringBuilder(description))
//        |> string

    let inline cleanDescription (obs:Observation) = obs |> foo

    let ``Taylor / product description length`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs
                |> cleanDescription
                |> String.length
                |> float

    let attributesDescription (obs:Observation) =

        obs.Product.Attributes
        |> Seq.map (fun kv ->
            match (kv.Value.ToLowerInvariant()) with
            | "yes" -> kv.Key // if true attrib, include attrib name
            | "no"  -> ""
            | _     ->
                match (kv.Key.StartsWith "bullet") with
                | true -> kv.Value
                | false -> sprintf "%s %s" kv.Key kv.Value)

    let inline containedIn (input : string) (word : string) = input.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0
    
    let ``Taylor / attributes not in description`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                // deduped
                let desc = 
                    obs 
                    |> cleanDescription
                    |> standardizeMeasures
                attributesDescription obs
                |> Seq.filter (containedIn desc >> not)
                |> String.concat " "
                |> String.length
                |> float

    let ``Taylor / search terms in title`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let title = obs.Product.Title |> wordTokenizer |> uniques |> Set.map stem
                let terms = obs.SearchTerm |> wordTokenizer |> uniques |> Set.map stem
                Set.intersect title terms |> Set.count |> float
//                let title = obs.Product.Title |> standardizeMeasures
//                obs.SearchTerm
//                |> standardizeMeasures
//                |> whiteSpaceTokenizer
//                |> Array.distinct
//                |> Array.filter (isMatch title)
//                |> Array.length
//                |> float

    let ``Taylor / % search terms in title`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let title = obs.Product.Title |> wordTokenizer |> uniques |> Set.map stem
                let terms = obs.SearchTerm |> wordTokenizer |> uniques |> Set.map stem
                let inter = Set.intersect title terms |> Set.count |> float
                inter / float terms.Count

//                let title = obs.Product.Title |> standardizeMeasures
//                let searchTerms = 
//                    obs.SearchTerm
//                    |> standardizeMeasures
//                    |> whiteSpaceTokenizer
//                    |> Array.distinct
//                let total = float searchTerms.Length
//                searchTerms
//                |> Array.filter (isMatch title)
//                |> Array.length
//                |> float
//                |> fun matches -> matches / total

    let ``Taylor / search terms in description`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let desc = obs.Product.Description |> wordTokenizer |> uniques |> Set.map stem
                let terms = obs.SearchTerm |> wordTokenizer |> uniques |> Set.map stem
                Set.intersect desc terms |> Set.count |> float

//                let description = 
//                    obs 
//                    |> cleanDescription
//                    |> standardizeMeasures
//                obs.SearchTerm
//                |> standardizeMeasures
//                |> whiteSpaceTokenizer
//                |> Array.distinct
//                |> Array.filter (isMatch description)
//                |> Array.length
//                |> float

    let ``Taylor / % search terms in description`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let desc = obs.Product.Description |> wordTokenizer |> uniques |> Set.map stem
                let terms = obs.SearchTerm |> wordTokenizer |> uniques |> Set.map stem
                let inter = Set.intersect desc terms |> Set.count |> float
                inter / float terms.Count
//                let description = 
//                    obs 
//                    |> cleanDescription
//                    |> standardizeMeasures
//                let searchTerms = 
//                    obs.SearchTerm
//                    |> standardizeMeasures
//                    |> whiteSpaceTokenizer
//                    |> Array.distinct
//                let total = float searchTerms.Length
//                searchTerms
//                |> Array.filter (isMatch description)
//                |> Array.length
//                |> float
//                |> fun matches -> matches / total

    let ``Taylor / search terms in attributes`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let desc = obs.Product.Description |> wordTokenizer |> uniques |> Set.map stem
                let terms = obs.SearchTerm |> wordTokenizer |> uniques |> Set.map stem
                let atts = attributesDescription obs |> String.concat " " |> wordTokenizer |> uniques |> Set.map stem
                Set.intersect desc terms |> Set.intersect atts |> Set.count |> float
//
//
//
//                let desc = 
//                    obs
//                    |> cleanDescription
//                    |> standardizeMeasures
//                let deduped = 
//                    attributesDescription obs
//                    |> Seq.filter (containedIn desc >> not)
//                    |> String.concat " "
//                    |> standardizeMeasures
//                obs.SearchTerm
//                |> standardizeMeasures
//                |> whiteSpaceTokenizer
//                |> Array.distinct
//                |> Array.filter (isMatch deduped)
//                |> Array.length
//                |> float
    
    let ``Taylor / search terms matched`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let description = 
                    obs 
                    |> cleanDescription
                    |> standardizeMeasures
                let title = obs.Product.Title |> standardizeMeasures
                let attributes = 
                    attributesDescription obs
                    |> String.concat " "
                    |> standardizeMeasures
                let words = String.concat " " [ description; title; attributes ]
                obs.SearchTerm
                |> standardizeMeasures
                |> whiteSpaceTokenizer
                |> Array.distinct
                |> Array.filter (isMatch words)
                |> Array.length
                |> float

    let ``Taylor / matching last search term and last title word`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let searchTerms = 
                    obs.SearchTerm 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                let titleTerms = 
                    obs.Product.Title 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                if (isMatch searchTerms.[searchTerms.Length - 1] titleTerms.[titleTerms.Length - 1])
                then 1.0
                else 0.0

    let ``Taylor / seq matching search terms and title terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let searchTerms = 
                    obs.SearchTerm 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                let titleTerms = 
                    obs.Product.Title 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                (searchTerms, titleTerms)
                ||> Seq.zip
                |> Seq.takeWhile (fun (s,t) -> isMatch s t)
                |> Seq.length
                |> float

    let ``Taylor / rev seq matching search terms and title terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let searchTerms = 
                    obs.SearchTerm 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                    |> Array.rev
                let titleTerms = 
                    obs.Product.Title 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                    |> Array.rev
                (searchTerms, titleTerms)
                ||> Seq.zip
                |> Seq.takeWhile (fun (s,t) -> isMatch s t)
                |> Seq.length
                |> float

    let ``Taylor / search terms vs title position score`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let searchTerms = 
                    obs.SearchTerm 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                let titleTerms = 
                    obs.Product.Title 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                    |> Array.rev
                let len = float (titleTerms.Length)
                searchTerms 
                |> Seq.map (fun w -> 
                    titleTerms 
                    |> Array.tryFindIndex (isMatch w) 
                    |> Option.map (fun x -> float x/len))
                |> Seq.choose id
                |> fun xs -> 
                    if Seq.isEmpty xs 
                    then 0. 
                    else Seq.average xs

    let ``Taylor / product has attributes`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                if obs.Product.Attributes.Count > 0 then 1. else 0.

    let ``Taylor / attribute names found in search terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let searchTerms = 
                    obs.SearchTerm 
                    |> standardizeMeasures
                    |> whiteSpaceTokenizer
                let matchedAttributeNames = 
                    obs.Product.Attributes
                    |> Seq.filter (fun kv -> kv.Key.StartsWith "bullet" |> not)
                    |> Seq.map (fun kv -> kv.Key |> standardizeMeasures)
                    |> Seq.filter (fun name -> searchTerms |> Seq.exists (isMatch name))
                    |> Seq.length
                float matchedAttributeNames

    // not the same, hugely simplified
    let ``Taylor / brand match`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                match obs.Product.Attributes.TryFind brandAttribute with
                | None -> 0.
                | Some(b) -> 
                    if containedIn obs.SearchTerm b then 1. else 0.    
                
    let ``number of attributes`` : FeatureLearner =
        fun sample ->
            fun obs -> obs.Product.Attributes.Count |> float

    let ``number of non-bullet attributes`` : FeatureLearner =
        fun sample ->
            fun obs ->
                obs.Product.Attributes
                |> Map.filter (fun key _ -> not (key.Contains "bullet"))
                |> Seq.length
                |> float

    let ``number of attributes log`` : FeatureLearner =
        fun sample ->
            fun obs -> obs.Product.Attributes.Count + 1 |> float |> log

    let ``number of attributes squared`` : FeatureLearner =
        fun sample ->
            fun obs -> obs.Product.Attributes.Count |> float |> fun x -> pown x 2

    let ``no attributes`` : FeatureLearner =
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.Count = 0 then 1. else 0.

    let ``single word search`` : FeatureLearner =
        fun sample ->
            fun obs ->
                wordTokenizer obs.SearchTerm
                |> uniques
                |> Set.count
                |> fun x -> if x = 1 then 1. else 0.

    let ``2 - 5 words search`` : FeatureLearner =
        fun sample ->
            fun obs ->
                wordTokenizer obs.SearchTerm
                |> uniques
                |> Set.count
                |> fun x -> if (x > 1 && x < 6) then 1. else 0.

    let ``10 words or more search`` : FeatureLearner =
        fun sample ->
            fun obs ->
                wordTokenizer obs.SearchTerm
                |> uniques
                |> Set.count
                |> fun x -> if (x > 10) then 1. else 0.


    let ``product with no brand`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match (obs.Product.Attributes.TryFind brandAttribute) with
                | None -> 1.
                | Some(brand) -> 0.

    let ``brand matches search terms`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match (obs.Product.Attributes.TryFind brandAttribute) with
                | None -> 0.
                | Some(brand) ->
                    if obs.SearchTerm.ToLowerInvariant().Contains(brand.ToLowerInvariant())
                    then 1.
                    else -1.

    let ``search terms and title % word intersection`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> lowerCase |> wordTokenizer |> uniques |> Set.map stem
                let title = obs.Product.Title |> lowerCase |> wordTokenizer |> uniques |> Set.map stem
                let longest = max terms.Count title.Count
                let intersect = Set.intersect terms title |> Set.count
                float intersect / float longest

    let ``words in title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                obs.Product.Title |> wordTokenizer |> uniques |> Set.count |> float

    let ``words in description`` : FeatureLearner =
        fun sample ->
            fun obs ->
                obs.Product.Description |> wordTokenizer |> uniques |> Set.count |> float

    let ``% search terms in description`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> lowerCase |> wordTokenizer |> uniques
                let desc = obs.Product.Description |> whiteSpaceTokenizer |> uniques
                let count =
                    terms
                    |> Set.fold (fun count word -> count + (desc |> Seq.filter (isMatch word) |> Seq.length)) 0
                float count / float terms.Count

    let ``search terms in title, order weighted`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms =
                    obs.SearchTerm
                    |> whiteSpaceTokenizer
                    |> Array.mapi (fun i word -> word, 1. / (pown 2. i))
                let title = obs.Product.Title |> whiteSpaceTokenizer
                let score =
                    terms
                    |> Array.fold (fun acc (word,weight) ->
                        if title |> Array.exists(isMatch word) then acc + weight else acc) 0.
                score

    let ``search terms in title, reverse order weighted`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms =
                    obs.SearchTerm
                    |> whiteSpaceTokenizer
                    |> Array.rev
                    |> Array.mapi (fun i word -> word, 1. / (pown 2. i))
                let title = obs.Product.Title |> whiteSpaceTokenizer
                let score =
                    terms
                    |> Array.fold (fun acc (word,weight) ->
                        if title |> Array.exists(isMatch word) then acc + weight else acc) 0.
                score

    let ``matching voltage`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match (findMeasure pVolts obs.SearchTerm) with
                | None -> 0.
                | Some(v) ->
                    match (findMeasure pVolts obs.Product.Title) with
                    | None -> 0.0
                    | Some(r) -> closeness 0.5 v r

    let ``matching wattage`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match (findMeasure pWatts obs.SearchTerm) with
                | None -> 0.
                | Some(v) ->
                    match (findMeasure pWatts obs.Product.Title) with
                    | None -> 0.0
                    | Some(r) -> closeness 0.5 v r

    let ``matching amperage`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match (findMeasure pAmps obs.SearchTerm) with
                | None -> 0.
                | Some(v) ->
                    match (findMeasure pAmps obs.Product.Title) with
                    | None -> 0.0
                    | Some(r) -> closeness 0.5 v r

    let ``matching gallons`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match (findMeasure pGallons obs.SearchTerm) with
                | None -> 0.
                | Some(v) ->
                    match (findMeasure pGallons obs.Product.Title) with
                    | None -> 0.0
                    | Some(r) -> closeness 0.5 v r

    let ``matching pounds`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match (findMeasure pPounds obs.SearchTerm) with
                | None -> 0.
                | Some(v) ->
                    match (findMeasure pPounds obs.Product.Title) with
                    | None -> 0.0
                    | Some(r) -> closeness 0.5 v r


//    let ``matches with attributes not in description`` : FeatureLearner =
//        fun sample ->
//            fun obs ->
//                let terms = obs.SearchTerm |> whiteSpaceTokenizer
//                let desc = obs.Product.Description |> whiteSpaceTokenizer
//                attributesDescription obs
//                |> Seq.filter (fun att -> )

    let ``count of matching attribute names`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer
                obs.Product.Attributes
                |> Seq.filter (fun kv -> not (kv.Key.StartsWith "Bullet"))
                |> Seq.sumBy (fun kv ->
                    if (hasMatch (kv.Key |> whiteSpaceTokenizer)) terms then 1. else 0.)

    let ``unmatched search words in title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer
                let title = obs.Product.Title |> whiteSpaceTokenizer
                if hasMatch terms title then 0. else 1.

