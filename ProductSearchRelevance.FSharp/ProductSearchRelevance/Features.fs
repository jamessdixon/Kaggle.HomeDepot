namespace HomeDepot

module Features = 

    open System
    open System.Text.RegularExpressions
    open HomeDepot.Model
    open FParsec

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

    let matchWords = Regex(@"\w+",RegexOptions.Compiled)

    let wordTokenizer (text:string) =
        text
        |> matchWords.Matches
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> Set.ofSeq

    // given a requested value and the result
    // return a value of 1.0 for a perfect match,
    // - 1.0 for a bad match
    let closeness (tolerance:float) requested result = 
        1. - 2. * (abs (requested - result) / ((1.0 - tolerance) * max requested result))

    (* 
    // illustration
    let x = 10.0
    let tol = 0.25
    // this is always -1.0, regardless of tol
    closeness tol x (x * tol)     
    closeness tol x (x / tol)     
    *)

    let spaceOrDash :Parser<unit,unit> = 
        (skipChar '-') <|> spaces 

    let pFraction : Parser<float,unit> = 
        tuple3 
            (pint32 .>> spaceOrDash)
            (pint32 .>> spaces .>> pstring "/" .>> spaces) 
            pint32
        |>> fun (x,y,z) -> float x + float y / float z

    // basic units / no fractions

    let pVolts : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "volt" <|> pstring "V" .>> (eof <|> spaces1))
    let pWatts : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "watt" <|> pstring "W" .>> (eof <|> spaces1))

    let findMeasure (parser:Parser<float,unit>) (text:string) =
        let last = text.Length - 1
        let rec search i =
            if i = last 
            then None
            elif (Char.IsDigit text.[i])
            then
                match run parser (text.Substring(i)) with
                | Success(x,_,_) -> Some x
                | Failure(_) -> search (i+1)
            else 
                search (i+1)
        search 0
        
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

    let ``matching voltage`` : FeatureLearner = 
        fun sample ->
            fun obs -> 
                match (findMeasure pVolts obs.SearchTerm) with
                | None -> 0.
                | Some(v) ->
                    match (findMeasure pVolts obs.Product.Title) with
                    | None -> -1.0
                    | Some(r) -> closeness 0.5 v r

    let ``matching wattage`` : FeatureLearner = 
        fun sample ->
            fun obs -> 
                match (findMeasure pWatts obs.SearchTerm) with
                | None -> 0.
                | Some(v) ->
                    match (findMeasure pVolts obs.Product.Title) with
                    | None -> -1.0
                    | Some(r) -> closeness 0.5 v r
