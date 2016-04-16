namespace HomeDepot

module Features =

    open System

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

    let brandAttribute = "MFG Brand Name" |> preprocess
    let productWeight = "product weight (lb.)" |> preprocess
    let productLength = "product length (in.)" |> preprocess

    let attributesDescription (obs:Observation) =
        obs.Product.Attributes
        |> Seq.map (fun kv ->
            match (kv.Value |> lowerCase) with
            | "yes" -> kv.Key // if true attrib, include attrib name
            | "no"  -> ""
            | _     ->
                match (kv.Key.StartsWith "bullet") with
                | true -> kv.Value
                | false -> sprintf "%s %s" kv.Key kv.Value)

    (*
    Feature definitions
    *)

    let uniqueStems = whiteSpaceTokenizer >> uniques >> Set.map stem
    let inline isMatch (word:string) term = distance word term <= word.Length / 4
    let getMatches words terms =
        terms |> Seq.where (fun t -> words |> Seq.exists (fun w -> isMatch w t))

    let softMatch (word1:string) (word2:string) =
        if isMatch word1 word2
        then
            let l1 = word1.Length |> float
            let l2 = word2.Length |> float
            min l1 l2 / max l1 l2
        else 0.

    let matchCount terms words =
        let terms = terms |> uniqueStems
        let words = words |> uniqueStems
        getMatches words terms |> Seq.length |> float

    let matchRatio terms words =
        let matches = matchCount terms words
        let termsCount = terms |> uniqueStems |> Seq.length
        float matches / float termsCount

    let ``Unique search terms matched in title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                matchCount obs.SearchTerm obs.Product.Title

    let ``% unique search terms matched in title`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                matchRatio obs.SearchTerm obs.Product.Title

    let ``% unique exact search terms matched in title`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> uniques |> Set.map stem
                let title = obs.Product.Title |> whiteSpaceTokenizer |> uniques |> Set.map stem
                let intersect = Set.intersect terms title
                float intersect.Count / float terms.Count

    let ``softmatch between search terms and title`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let terms = uniqueStems obs.SearchTerm
                let words = uniqueStems obs.Product.Title
                terms |> Seq.sumBy (fun t -> words |> Seq.sumBy (softMatch t))

    let ``Unique search terms matched in description`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                matchCount obs.SearchTerm obs.Product.Description

    let ``Search term has color`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let color = Colors.colorMatch obs.SearchTerm
                if color.IsSome then 1. else 0.

    let ``Title has color`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let color = Colors.colorMatch obs.Product.Title
                if color.IsSome then 1. else 0.

    let ``Search and title have matching color`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let titleColor = Colors.colorMatch obs.Product.Title
                let queryColor = Colors.colorMatch obs.SearchTerm
                match titleColor, queryColor with
                | Some t, Some q when t = q -> 1.
                | _ -> 0.

    // weak
    let ``% unique search terms matched in description`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                matchRatio obs.SearchTerm obs.Product.Description

    let ``frequency weighted search terms matched in description`` : FeatureLearner =
        let frequencies = 
            let terms = seq {
                yield! trainset |> Seq.map (fun (l,o) -> o.Product.Description)
                yield! testset |> Seq.map (fun o -> o.Product.Description) }               
            terms
            |> Seq.distinct
            |> Seq.map (whiteSpaceTokenizer >> Array.map stem)
            |> Seq.collect id
            |> Seq.countBy id
            |> Seq.map (fun (word,count) -> word, 1. / float count)
            |> Map.ofSeq

        fun sample ->
            fun obs -> 
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> uniques |> Set.map stem
                let desc = obs.Product.Description |> whiteSpaceTokenizer |> Array.map stem
                desc
                |> Array.sumBy (fun word ->
                    if terms.Contains word 
                    then frequencies.[word] 
                    else 0.)

    let ``Unique search terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs.SearchTerm |> whiteSpaceTokenizer |> uniques |> Set.count |> float

    // weak
    let ``Duplicate search terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                let nonUniques = obs.SearchTerm |> whiteSpaceTokenizer
                let deduped = nonUniques |> uniques
                if nonUniques.Length = deduped.Count then 0. else 1.

    let ``Search terms length`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                obs.SearchTerm.Length |> float
    
    open System.Text.RegularExpressions
    let stopWordRegex = Regex(@"(?:\b|\s)(?:the|and|inc|products)(?:\s|\b)", RegexOptions.Compiled)
    let cleanBrand (brand:string) =
        stopWordRegex.Replace(brand, " ").Trim()

    let brands =
        attributes
        |> Map.filter (fun k _ -> k = brandAttribute)
        |> Seq.collect (fun (KeyValue(_,v)) -> v)
        |> Seq.map cleanBrand
        |> Set.ofSeq

    let getQueryBrand (query:string) =
        let query = cleanBrand query
        if brands.Contains query then
            Some query
        else
            brands
            |> Seq.tryFind (fun b -> query.StartsWith(b + " ") || query.EndsWith(" " + b))

    let getProductBrand obs =
        match (obs.Product.Attributes.TryFind brandAttribute) with
        | Some b -> Some (b |> cleanBrand)
        | None -> // fallback to title
            match getQueryBrand (cleanBrand obs.Product.Title) with
            | Some b -> Some b
            | None -> None

    let ``Query brand matches title brand`` : FeatureLearner =
        fun sample ->
            fun obs ->
                match getQueryBrand obs.SearchTerm with
                | Some qb ->
                    let titleBrand = getProductBrand obs
                    match titleBrand with
                    | Some t when qb = t -> 1. // match
                    | Some _ -> -1. // mismatch
                    | _ -> 0.
                | None -> 0.

    let ``Brand match in search terms`` : FeatureLearner =
        fun sample ->
            fun obs -> 
                match (obs.Product.Attributes.TryFind brandAttribute) with
                | None -> 0.
                | Some(brand) ->
                    let name = brand |> whiteSpaceTokenizer |> uniques
                    let terms = obs.SearchTerm |> whiteSpaceTokenizer |> uniques
                    let inter = Set.intersect name terms
                    (float inter.Count) / (float name.Count)

    let ``First search terms and title words match`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let term = (obs.SearchTerm |> whiteSpaceTokenizer).[0] |> stem
                let word = (obs.Product.Title |> whiteSpaceTokenizer).[0] |> stem
                if isMatch term word then 1. else 0.

    let ``Last search terms and first title words match`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer
                let last = terms.[terms.Length - 1] |> stem
                let word = (obs.Product.Title |> whiteSpaceTokenizer).[0] |> stem
                if isMatch last word then 1. else 0.

    let ``Last search terms and title words match`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer
                let lastTerm = terms.[terms.Length - 1] |> stem
                let words = (obs.Product.Title |> whiteSpaceTokenizer)
                let lastWord = words.[words.Length - 1] |> stem
                if isMatch lastTerm lastWord then 1. else 0.

    let ``Title contains last search term`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer
                let lastTerm = terms.[terms.Length - 1] |> stem
                let words = (obs.Product.Title |> whiteSpaceTokenizer |> uniques |> Set.map stem)
                if words |> Set.contains lastTerm then 1. else 0.

    let ``Product Id`` : FeatureLearner =
        fun sample ->
            fun obs ->
                obs.Product.UID |> float

    // weak. version from original with title length is weaker
    let ``Position of search terms in title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> Array.map stem
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem
                let len = terms.Length
                let pos =
                    seq { for term in terms -> title |> Array.tryFindIndex ((=) term) }
                    |> Seq.choose (fun x -> 
                        match x with 
                        | None -> None
                        | Some(x) -> Some <| float (len-x) / float len)
                    |> Array.ofSeq
                if pos.Length = 0 then 0. else pos |> Array.average

    //  version from original with title length is weaker
    let ``Reverse position of search terms in title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> Array.map stem
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem |> Array.rev
                let len = terms.Length
                let pos =
                    seq { for term in terms -> title |> Array.tryFindIndex ((=) term) }
                    |> Seq.choose (fun x -> 
                        match x with 
                        | None -> None
                        | Some(x) -> Some <| float (len-x) / float len)
                    |> Array.ofSeq
                if pos.Length = 0 then 0. else pos |> Array.average

    // weak
    let ``Unmatched search terms in title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> uniques |> Set.map stem
                let title = obs.Product.Title |> whiteSpaceTokenizer |> uniques |> Set.map stem
                let inter = Set.intersect terms title
                inter.Count - terms.Count |> float

    let ``Longest matching seq between search terms and title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> Array.map stem
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem
                (terms, title) ||> Seq.zip |> Seq.takeWhile (fun (a,b) -> isMatch a b) |> Seq.length |> float

    // different from original but works better
    let ``Longest backwards matching seq between search terms and title`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> Array.map stem |> Array.rev
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem |> Array.rev
                (terms, title) ||> Seq.zip |> Seq.takeWhile (fun (a,b) -> isMatch a b) |> Seq.length |> float
    
    let ``Number of non-bullet attributes`` : FeatureLearner =
        fun sample ->
            fun obs ->
                obs.Product.Attributes |> Seq.filter (fun kv -> kv.Key.StartsWith("bullet") |> not) |> Seq.length |> float

    // weak
    let ``Number of bullet attributes`` : FeatureLearner =
        fun sample ->
            fun obs ->
                obs.Product.Attributes |> Seq.filter (fun kv -> kv.Key.StartsWith("bullet")) |> Seq.length |> float

    // weak
    let ``Number of attributes`` : FeatureLearner =
        fun sample ->
            fun obs ->
                obs.Product.Attributes.Count |> float

    let ``Number of matching attribute names`` : FeatureLearner =
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> Array.map stem |> uniques
                obs.Product.Attributes
                |> Seq.filter (fun kv -> 
                    let keyWords = kv.Key |> whiteSpaceTokenizer |> Array.map stem |> uniques
                    (Set.intersect keyWords terms).Count > 0)
                |> Seq.length
                |> float

    let ``Search terms contain number`` : FeatureLearner =
        fun sample ->
            fun obs ->
                if obs.SearchTerm |> Seq.exists (Char.IsDigit) then 1. else 0.

    let ``Close product weight`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let weight = obs.Product.Attributes.TryFind productWeight
                match weight with
                | None -> 0.
                | Some(weightInfo) ->
                    let ws = extractBasicNumbers weightInfo
                    match ws with
                    | [] -> 0.
                    | w::_ ->
                        let ns = extractBasicNumbers obs.SearchTerm
                        match ns with
                        | [] -> 0.
                        | _ ->
                            let best = ns |> Seq.minBy (fun n -> abs (n-w) / n)
                            if best < 0.25 then 1. else 0.

    let ``Has weight`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                    let weight = obs.Product.Attributes.TryFind productWeight
                    match weight with
                    | None -> 0.
                    | Some(w) -> 1.

    let ``Close product length`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let length = obs.Product.Attributes.TryFind productLength
                match length with
                | None -> 0.
                | Some(lengthInfo) ->
                    let ls = extractBasicNumbers lengthInfo
                    match ls with
                    | [] -> 0.
                    | l::_ ->
                        let ns = extractBasicNumbers obs.SearchTerm
                        match ns with
                        | [] -> 0.
                        | _ ->
                            let best = ns |> Seq.minBy (fun n -> abs (n-l) / n)
                            if best < 0.25 then 1. else 0.

    let ``Has length`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let length = obs.Product.Attributes.TryFind productLength
                match length with
                | None -> 0.
                | Some(w) -> 1.

    let ``Search terms specificity`` : FeatureLearner = 
        let frequencies = 
            let terms = seq {
                yield! trainset |> Seq.map (fun (l,o) -> o.SearchTerm)
                yield! testset |> Seq.map (fun o -> o.SearchTerm) }               
            terms
            |> Seq.distinct
            |> Seq.map (fun term -> term |> whiteSpaceTokenizer |> Array.map stem)
            |> Seq.collect id
            |> Seq.countBy id
            |> Seq.map (fun (word,count) -> word, 1. / float count)
            |> Map.ofSeq
        fun sample ->
            fun obs ->
                obs.SearchTerm 
                |> whiteSpaceTokenizer 
                |> Array.map stem
                |> Array.averageBy (fun word -> 
                    match (frequencies.TryFind word) with
                    | Some(x) -> x
                    | None -> 0.)

    let ``Specificity weighted Search terms match`` : FeatureLearner = 
        let frequencies = 
            let terms = seq { 
                yield! trainset |> Seq.map (fun (l,o) -> o.SearchTerm)
                yield! testset |> Seq.map (fun o -> o.SearchTerm) }
            terms
            |> Seq.distinct
            |> Seq.map (fun title -> title |> whiteSpaceTokenizer|> Array.map stem)
            |> Seq.collect id
            |> Seq.countBy id
            |> Seq.map (fun (word,count) -> word, 1. / float count)
            |> Map.ofSeq
        fun sample ->
            fun obs ->
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem
                obs.SearchTerm
                |> whiteSpaceTokenizer
                |> Array.map stem 
                |> Array.averageBy (fun word -> 
                    match (frequencies.TryFind word) with
                    | Some(x) -> if title |> Array.contains word then x else 0.
                    | None -> 0.)

    let ``Frequency weighted title match`` : FeatureLearner = 
        let frequencies = 
            let titles = seq { 
                yield! trainset |> Seq.map (fun (l,o) -> o.Product.Title)
                yield! testset |> Seq.map (fun o -> o.Product.Title) }
            titles
            |> Seq.distinct
            |> Seq.map (fun title -> title |> whiteSpaceTokenizer |> Array.map stem)
            |> Seq.collect id
            |> Seq.countBy id
            |> Seq.map (fun (word,count) -> word, 1. / float count)
            |> Map.ofSeq
        fun sample ->
            fun obs ->
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem
                obs.SearchTerm 
                |> whiteSpaceTokenizer 
                |> Array.map stem
                |> Array.averageBy (fun word -> 
                    match (frequencies.TryFind word) with
                    | Some(x) -> x
                    | None -> 0.)

    let ``Contains a surface`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if dimensions.IsMatch(obs.SearchTerm)
                then 1. else 0.

    let ``Brand mismatch`` : FeatureLearner = 
        let brands = 
            seq { 
                yield! trainset |> Seq.map (fun (l,o) -> o.Product.Attributes.TryFind brandAttribute)
                yield! testset |> Seq.map (fun o -> o.Product.Attributes.TryFind brandAttribute) }
            |> Seq.choose id
            |> Seq.map (whiteSpaceTokenizer >> uniques)
            |> Seq.distinct
            |> Set.ofSeq
        fun sample ->
            fun obs ->
                let brand = obs.Product.Attributes.TryFind brandAttribute
                match brand with
                | None -> 0.
                | Some(brandName) ->
                    let brandName = brandName |> whiteSpaceTokenizer |> uniques
                    let terms = obs.SearchTerm |> whiteSpaceTokenizer |> uniques
                    let inSearch = brands |> Set.filter (fun x -> (Set.intersect x terms).Count > 0)
                    if (inSearch |> Set.contains brandName) then 0. else 1.

    let ``Product type match`` : FeatureLearner = 
        // extract word weights for each product type category
        let types = 
            attributes 
            |> Map.filter (fun key value -> key.EndsWith "product type")
            |> Map.map (fun key values ->
                let words =
                    values 
                    |> Seq.collect whiteSpaceTokenizer
                    |> Seq.map stem
                    |> Seq.countBy id
                let largest = float (words |> Seq.map snd |> Seq.max)
                words
                |> Seq.map (fun (w,c) -> w, float c / largest)
                |> Map.ofSeq)

        fun sample ->
            fun obs ->
                let productTypes = 
                    obs.Product.Attributes
                    |> Map.filter (fun k v -> k.EndsWith "product type")
                    |> Map.map (fun k v -> types.[k])
                    |> Seq.collect (fun kv -> kv.Value)
                    |> Seq.map (fun kv -> kv.Key, kv.Value)
                obs.SearchTerm
                |> whiteSpaceTokenizer
                |> Seq.map stem
                |> Seq.fold (fun acc word ->
                    let score = 
                        productTypes 
                        |> Seq.fold (fun s (w,x) ->
                            if w = word then s + x else s) 0.
                    acc + score) 0.

    let ``Bigrams title match`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let titleBigrams = 
                    obs.Product.Title 
                    |> whiteSpaceTokenizer 
                    |> Array.map stem 
                    |> Array.pairwise
                let searchBigrams = 
                    obs.SearchTerm 
                    |> whiteSpaceTokenizer 
                    |> Array.map stem 
                    |> Array.pairwise
                searchBigrams
                |> Seq.fold (fun acc term -> 
                    if titleBigrams |> Array.contains term
                    then acc + 1. else acc) 0.

    // weak
    let ``Trigrams title match`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let titleTrigrams = 
                    obs.Product.Title 
                    |> whiteSpaceTokenizer 
                    |> Array.map stem 
                    |> Array.windowed 3
                let searchTrigrams = 
                    obs.SearchTerm 
                    |> whiteSpaceTokenizer 
                    |> Array.map stem 
                    |> Array.windowed 3
                searchTrigrams
                |> Seq.fold (fun acc term -> 
                    if titleTrigrams |> Array.contains term
                    then acc + 1. else acc) 0.

    // weak
    let ``certifications and listings`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.ContainsKey "certifications and listings"
                then 1.
                else 0.

    // weak
    let ``energy star certified`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.ContainsKey "energy star certified"
                then 1.
                else 0.

    // weak
    let ``material`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.ContainsKey "material"
                then 1.
                else 0.

    // weak
    let ``product length in`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.ContainsKey "product length in"
                then 1.
                else 0.

    // weak
    let ``product width in`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.ContainsKey "product width in"
                then 1.
                else 0.

    // weak
    let ``product height in`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.ContainsKey "product height in"
                then 1.
                else 0.

    // weak
    let ``product depth in`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.ContainsKey "product depth in"
                then 1.
                else 0.

    let ``Word bigrams title match`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let titleBigrams = 
                    obs.Product.Title 
                    |> whiteSpaceTokenizer 
                    |> Array.map (fun w -> stem w, pureWord w) 
                    |> Array.pairwise
                    |> Array.filter (fun ((_,w1), (_,w2)) -> w1 && w2)
                let searchBigrams = 
                    obs.SearchTerm 
                    |> whiteSpaceTokenizer 
                    |> Array.map (fun w -> stem w, pureWord w) 
                    |> Array.pairwise
                    |> Array.filter (fun ((_,w1), (_,w2)) -> w1 && w2)
                searchBigrams
                |> Seq.fold (fun acc term -> 
                    if titleBigrams |> Array.contains term
                    then acc + 1. else acc) 0.

    let ``Frequency weighted title match 2`` : FeatureLearner = 
        let frequencies = 
            let titles = seq { 
                yield! trainset |> Seq.map (fun (l,o) -> o.Product.Title)
                yield! testset |> Seq.map (fun o -> o.Product.Title) }
            titles
            |> Seq.distinct
            |> Seq.map (fun title -> title |> whiteSpaceTokenizer |> Array.map stem)
            |> Seq.collect id
            |> Seq.filter pureWord
            |> Seq.countBy id
            |> Seq.map (fun (word,count) -> word, 1. / float count)
            |> Map.ofSeq
        fun sample ->
            fun obs ->
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem
                obs.SearchTerm 
                |> whiteSpaceTokenizer 
                |> Array.map stem
                |> Array.filter pureWord
                |> Array.sumBy (fun word -> 
                    match (frequencies.TryFind word) with
                    | Some(x) -> x
                    | None -> 0.)

    let ``Search vs Title similarity`` : FeatureLearner = 
        let titleFrequencies = 
            let titles = seq { 
                yield! trainset |> Seq.map (fun (l,o) -> o.Product.Title)
                yield! testset |> Seq.map (fun o -> o.Product.Title) }
            titles
            |> Seq.distinct
            |> Seq.map (fun title -> title |> whiteSpaceTokenizer |> Array.map stem)
            |> Seq.collect id
            |> Seq.countBy id
            |> Seq.map (fun (word,count) -> word, 1. / float count)
            |> Map.ofSeq
        let searchFrequencies = 
            let searches = seq { 
                yield! trainset |> Seq.map (fun (l,o) -> o.SearchTerm)
                yield! testset |> Seq.map (fun o -> o.SearchTerm) }
            searches
            |> Seq.distinct
            |> Seq.map (fun title -> title |> whiteSpaceTokenizer |> Array.map stem)
            |> Seq.collect id
            |> Seq.countBy id
            |> Seq.map (fun (word,count) -> word, 1. / float count)
            |> Map.ofSeq

        fun sample ->
            fun obs ->
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem |> uniques
                let search = obs.SearchTerm |> whiteSpaceTokenizer |> Array.map stem |> uniques
                similarity titleFrequencies searchFrequencies title search

    let ``Measure mismatch`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let title = 
                    obs.Product.Title 
                    |> whiteSpaceTokenizer 
                    |> Array.filter pureNumber
                    |> Array.map float
                let search = 
                    obs.SearchTerm 
                    |> whiteSpaceTokenizer 
                    |> Array.filter pureNumber
                    |> Array.map float
                if search.Length = 0 || title.Length = 0
                then 0.
                else
                    search 
                    |> Array.map (fun x ->
                        title 
                        |> Seq.map (fun t -> abs (t - x) / x)
                        |> Seq.min)
                    |> Seq.max

    // no improvement on leaderboard
    let ``Dimension information`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let xs = obs.SearchTerm.Contains (" x ")
                if xs
                then
                    let dimensions = 
                        obs.Product.Attributes 
                        |> Map.filter (fun k v -> 
                            k.Contains "height" 
                            || k.Contains "width"
                            || k.Contains "length") 
                        |> Seq.length
                    1. / (1. + float dimensions)
                else 0.

    let ``Has no attributes`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                if obs.Product.Attributes.Count = 0 then 1. else 0.

    let ``Title match weighted by position`` : FeatureLearner = 
        fun sample ->
            fun obs ->
                let terms = obs.SearchTerm |> whiteSpaceTokenizer |> Array.map stem
                let title = obs.Product.Title |> whiteSpaceTokenizer |> Array.map stem |> uniques
                let len = float (terms.Length)
                terms 
                |> Seq.mapi (fun i word -> 
                    if title.Contains word 
                    then float (i + 1) / float len
                    else 0.)
                |> Seq.sum
