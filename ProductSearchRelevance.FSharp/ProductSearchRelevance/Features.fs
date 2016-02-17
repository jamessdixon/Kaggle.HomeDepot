namespace HomeDepot

module Features = 

    open HomeDepot.Model

    type Feature = Observation -> float

    type FeatureLearner = Observation [] -> Feature

    let featurizer (featureLearners:FeatureLearner[]) sample =
        featureLearners 
        |> Array.map (fun learner -> learner sample)
     
    let extract (features:Feature[]) obs = 
        features |> Array.map (fun f -> f obs)

    (* 
    text manipulation
    TODO factor out / into StringUtils?
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

    (*
    Feature definitions
    *)
    
    let ``number of attributes`` : FeatureLearner =
        fun sample ->
            fun obs -> obs.Product.Attributes.Count |> float
    
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

    let ``words in search terms`` : FeatureLearner =
        fun sample ->
            fun obs -> wordTokenizer obs.SearchTerm |> Set.count |> float

    let ``single word search`` : FeatureLearner = 
        fun sample ->
            fun obs -> 
                wordTokenizer obs.SearchTerm 
                |> Set.count 
                |> fun x -> if x = 1 then 1. else 0.

    let brand = "MFG Brand Name"

    let ``product with no brand`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                match (obs.Product.Attributes.TryFind brand) with
                | None -> 1.
                | Some(brand) -> 0.

    let ``brand matches search terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                match (obs.Product.Attributes.TryFind brand) with
                | None -> 0.
                | Some(brand) ->
                    if obs.SearchTerm.ToLowerInvariant().Contains(brand.ToLowerInvariant())
                    then 1.
                    else -1.

    let ``search terms and title % word intersection`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let terms = obs.SearchTerm |> lower |> wordTokenizer
                let title = obs.Product.Title |> lower |> wordTokenizer
                let longest = max terms.Count title.Count
                let intersect = Set.intersect terms title |> Set.count
                float intersect / float longest

    let ``words in title`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs.Product.Title |> wordTokenizer |> Set.count |> float

    let ``words in description`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs.Product.Description |> wordTokenizer |> Set.count |> float

    let ``% search terms in description`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let terms = obs.SearchTerm |> lower |> wordTokenizer
                let desc = obs.Product.Description |> lower
                let count = terms |> Set.fold (fun count word -> if desc.Contains word then count + 1 else count) 0
                float count / float terms.Count

    let ``search terms in title, order weighted`` : FeatureLearner = 
        fun sample ->
            fun obs -> 
                let terms = 
                    obs.SearchTerm 
                    |> lower 
                    |> fun x -> x.Split ' ' 
                    |> Array.filter (fun x -> x <> "")
                    |> Array.mapi (fun i word -> word, 1. / (pown 2. i))
                let title = obs.Product.Title |> lower
                let score = 
                    terms 
                    |> Array.fold (fun acc (word,weight) -> 
                        if title.Contains word then acc + weight else acc) 0.
                score

    let ``search terms in title, reverse order weighted`` : FeatureLearner = 
        fun sample ->
            fun obs -> 
                let terms = 
                    obs.SearchTerm 
                    |> lower 
                    |> fun x -> x.Split ' ' 
                    |> Array.filter (fun x -> x <> "")
                    |> Array.rev
                    |> Array.mapi (fun i word -> word, 1. / (pown 2. i))
                let title = obs.Product.Title |> lower
                let score = 
                    terms 
                    |> Array.fold (fun acc (word,weight) -> 
                        if title.Contains word then acc + weight else acc) 0.
                score

