namespace HomeDepot

module Utilities =

    open System
    open System.Text.RegularExpressions
    open System.Net
    open System.Collections.Concurrent

    open Iveonik.Stemmers
    open FParsec

    (*
    Basic string operations
    *)

// cleansing

    let inline cleanHtml (txt:string) = WebUtility.HtmlDecode txt

    let inline lowerCase (txt:string) = txt.ToLowerInvariant()

    let manySpaces = Regex(@"\s+", RegexOptions.Compiled)
    let inline cleanSpaces (txt:string) = manySpaces.Replace(txt, " ")

    // processing numbers

    // remove , separating thousands
    let thousandsSeparator = Regex(@"(\d+),(?=\d{3}(\D|$))", RegexOptions.Compiled)
    let inline cleanThousands (txt:string) =
        thousandsSeparator.Replace(txt,"$1")

    let dots = Regex(@"([^0-9])\.|\.([^0-9]|$)", RegexOptions.Compiled)
    let inline cleanDots (txt:string) =
        dots.Replace(txt,"$1 $2")

    // misses A/C
    let fraction = Regex(@"([^0-9])\/|\/([^0-9]|$)", RegexOptions.Compiled)
    let inline cleanFractions (txt:string) =
        fraction.Replace(txt, "$1 $2")

    // nuke punctuation signs, except .
    // commas separating 1000s need to be handled first
    let punctuation = Regex(@"[!?;:,\[\]\(\)\+\-&#_ې۪�]", RegexOptions.Compiled)
    let inline cleanPunctuation (txt:string) = punctuation.Replace(txt, " ")

    // careful, symbols used for feet and inches
    let exclamation = Regex(@"['\""]", RegexOptions.Compiled)
    let cleanExclamation (txt:string) = exclamation.Replace(txt, " ")

    let dollars = Regex("([0-9])\s*\$ | \$\s*([0-9])", RegexOptions.Compiled)
    let cleanDollars (txt:string) = dollars.Replace(txt, "$1 dollars $2")

    let units = [
        "sq. ft.", @"(square|sq)\.?\s*('|ft\.|ft|foot|feet)"
        "cu. ft.", @"(cubic|cu)\.?\s*('|ft\.|ft|foot|feet)"
        "pct.", "%"
        "in.", @"""|inches|inch|in\.|in\s"
        "ft.", @"'|ft\.|ft|foot|feet"
        "lb.", @"pounds|pound|lbs|lb\.|lb"
        "gal.", @"gallons|gallon|gal\s|gal\."
        "oz.", @"ounces|ounce|oz\.|oz"
        "volt", @"volts|volt|V\s|v\s" // check 5-v
        "watt", @"watts|watt" // check 5 W vs. width?
        "amp", @"ampere|amps|amp\."
        "btu", @"BTU"
        "yd.", @"yards|yard|yd\.|yd"
        "mm", @"millimeters|millimeter|mm\.|mm\s"
        ]

    let unitsReplacers =
        units
        |> List.map (fun (replacement,pattern) ->
            let regex = Regex(sprintf "([0-9]\s*)(%s)" pattern, RegexOptions.Compiled)
            fun (txt:string) -> regex.Replace(txt, sprintf "$1 %s " replacement))

    let inline cleanUnits (txt:string) = 
        unitsReplacers 
        |> List.fold (fun result f -> f result) txt

    let multiply = Regex("([0-9\.\s])(\*|x|X|by\s)(\s*[0-9])", RegexOptions.Compiled)
    let inline cleanMultiply (txt:string) = multiply.Replace(txt, "$1 x $3")

    let inline letterNumber (text:string) =
        Regex.Replace(text, "([a-zA-Z])([0-9])", "$1 $2")

    let inline numberLetter (text:string) =
        Regex.Replace(text, "([0-9])([a-zA-Z])", "$1 $2")

    let lettersNumbers = [ 
        "zero", 0
        "one", 1
        "two", 2
        "tree", 3
        "four", 4
        "five", 5
        "six", 6
        "seven", 7
        "eight", 8
        "nine", 9 ]

    let inline replaceNumbers (txt:string) =
        lettersNumbers 
        |> Seq.fold (fun (t:string) (number,value) -> 
            t.Replace(sprintf " %s " number, sprintf " %i " value)) txt
        
    let trim (txt:string) = txt.Trim ()

    let substitutions = [
        "berh", "behr "
        "dewalr", "dewalt"
        "roybi", "ryobi"
        "ryobl", "ryobi"

        "outdoor", "outdoor " // for outdoortable
        "ourdoor", "outdoor"
        "flouresc", "fluoresc"
        "rustolem", "rustoleum"
        "rustollum", "rustoleum"
        "cieling", "ceiling"
        "repir", "repair"
        "hindge", "hinge"
        "toliet", "toilet"
        "americian", "american"
        "teir", "tier"
        "porcelian", "porcelain"
        "porcelin", "porcelain"
        "mosiac", "mosaic"

        "steele", "steel" 
        "shelve", "shelf" 
        "toliet", "toilet" 
        "glaciar","glacier" 
        "tiolet", "toilet" 
        "tolet", "toilet" 
        "repir", "repair" 
        "electic", "electric" 
        "porcelin","porcelain" 
        "riobi", "ryobi" 
        "cieling", "ceiling" 
        "celing", "ceiling" 
        "ceadar", "cedar" 
        "jeldwen","jeld wen" 
        "milwakee", "milwaukee" 
        "roybi", "ryobi" 
        "pain", "pane" 
        "vynal", "vinyl" 
        "aluminun", "aluminium" 
        "vlve", "valve" 
        "pice","piece" 
        "replacment", "replacement" 
        "ceder", "cedar" 
        "dooor", "door" 
        "garge", "garage" 
        "venner", "veneer" 
        "canopie","canopy" 
        "pressue", "pressure" 
        "trimer", "trimmer" 
        "nickle", "nickel" 
        "flexable", "flexible" 
        "selves", "shelves" 
        "koehler", "kohler" 
        "drils", "drills" 
        "treads", "threads" 
        "vacum", "vacuum" 
        "accesories", "accessories" 
        "fictures", "fixtures" 
        "robyi", "ryobi" 
        "upholstry", "upholstery" 
        "frenchdoor", "french door" 
        "dor", "door" 
        "shelfs", "shelves" 
        "pannel", "panel" 
        "hookups"," " 
        "rachet","ratchet" 
        "elec", "electric" 
        "airconditioner","air conditioner" 
        "fireglass"," " 
        "refrig", "refriger" // stemmed version
        "florecent", "fluorescent" 
        "porcelian", "porcelain" 
        "blad", "blade" 
        "rechargable", "rechargeable" 
        "sinl", "sink" 
        "infared", "infrared" 
        "blubs", "bulbs" 
        "vinal","vinyl" 
        "bosh", "bosch" 
        "boshe", "bosch" 
        "stainles","stainless" 
        "ligh", "light" 
        "lightbulbs","light bulbs" 
        "aircondition","air conditioner" 
        "temperture", "temperature" 
        "mapp", "map" 
        "vaccuum", "vacuum" 
        "mobilehome", "mobile home" 
        "pation", "patio" 
        "inx", "in. x " 
        "batterys", "batteries" 
        "sower", "shower" 
        "plans"," " 
        "sawall", "saw all" 
        "ele"," " 
        "barbque", "barbeque" 
        "comercial", "commercial" 
        "eletric", "electric" 
        "composit","composite" 
        "shelv", "shelves" 
        "kithen", "kitchen" 
        "kiddie", "kid" 
        "bisquit", "biscuit" 
        "nitch", "niche" 
        "dorr", "door" 
        "frigidare", "frigidaire" 
        "connecter", "connector" 
        "panals", "panels" 
        "shelfing", "shelving" 
        "electical", "electrical" 
        "brakets", "brackets" 
        "doorknob", "door knob" 
        "vigaro", "vigoro" 
        "presure", "pressure" 
        "multitool","multi tool" 
        "handel","handle" 
        "manuel","manual" 
        "underdeck", "under deck" 
        "vaccume","vaccum" 
        ]

    let inline cleanMisspellings (txt:string) =
        substitutions
        |> Seq.map (fun (before,after) -> sprintf " %s " before, sprintf " %s " after)
        |> Seq.fold (fun (acc:string) (before,after) -> acc.Replace(before,after)) txt

    let preprocess =
        cleanHtml 
        >> lowerCase
        >> cleanThousands 
        >> cleanDots
        >> cleanFractions
        >> cleanPunctuation
        >> letterNumber
        >> numberLetter
        >> replaceNumbers
        >> cleanDollars
        >> cleanUnits
        >> cleanMultiply
        >> cleanExclamation
        >> cleanMisspellings
        >> cleanSpaces
        >> trim

    let uniques (words:string[]) = words |> Set.ofArray

    let basicNumbers = Regex(@"(?<!/)(\d+)(\.\d+)?(?!/)", RegexOptions.Compiled)
    let extractBasicNumbers (text:string) =
        basicNumbers.Matches text 
        |> Seq.cast<Match> 
        |> Seq.map (fun x -> x.Value |> float) 
        |> Seq.toList
        
    (*
    Tokenization
    *)
    
    type Tokenizer = string -> string[]
    
    let whiteSpaceTokenizer : Tokenizer = fun text ->
        Regex.Split(text,@"\s+")

    let numbers = Regex(@"(?<float>(\d*\.\d+)|(\d+\.\d*))|(?<integer>\d*)\s+(?<numerator>\d+)\s*\/\s*(?<denominator>\d+)", RegexOptions.Compiled)

    let extractNumbers (text:string) =

        let results = numbers.Matches(text)
        
        results
        |> Seq.cast<Match>
        |> Seq.map (fun m -> 
            let flo = m.Groups.["float"].Value
            if flo <> "" then (float flo)
            else
                let lead = m.Groups.["integer"].Value
                let num = m.Groups.["numerator"].Value
                let den = m.Groups.["denominator"].Value
                if lead = "" 
                then (float num) / (float den)
                else (float lead) + (float num) / (float den))
        |> Seq.toList

    let dimensions = Regex @"(\d\s*)[xX](\s*\d)"

    let simplify (text:string) = Regex.Replace(text,@"[^\w]","")

    // Breaking malformed sentences in Description
    let pattern = """(^[a-z|0-9]+)[.!?]?([A-Z]\w+)|(^\w+)[.!?]([A-Z]\w+)"""

    let descriptionSentenceBreak (word:string) =
        let breaker = Regex(pattern)
        Regex.Split(word,pattern) |> Array.filter((<>) "")

    (* 
    //Crude validation for the sentence break
    let test =
        [
            "basic"
            "Basic"
            "BASIC"
            "beforeAfter"
            "BeforeAfter"
            "Before.After"
            "before.After"
            "123After"
            "123.45After"
            "beforeAFter"
            "beforeMyProduct"
        ]
        |> List.map descriptionSentenceBreak
    *)

    (*
    Stemmer
    *)
    
    let stemDictionary = ConcurrentDictionary<string, string>(StringComparer.OrdinalIgnoreCase)
    
    let stem word =
        stemDictionary.GetOrAdd(word, (fun s -> (EnglishStemmer()).Stem s))
    
    (*
    Old code, needs to be cleaned up
    *)

    // are 2 words identical?
    let isMatch (input:string) (word:string) =
        let word = word |> stem |> Regex.Escape
        let input = input |> stem |> Regex.Escape
        Regex.IsMatch(input, word, RegexOptions.IgnoreCase)

    // do 2 arrays of words have identical ones?
    let hasMatch words1 words2 =
        words1
        |> Array.exists (fun word1 ->
            words2
            |> Array.exists (fun word2 -> isMatch word1 word2))

    // how many times does an array of words contain a word?
    let matches words word =
        words
        |> Array.fold (fun count w -> if isMatch w word then count + 1 else count) 0

    // how many common words do 2 arrays of words have?
    let commonWords words1 words2 =
        words1 |> Array.sumBy (matches words2)

    // code formerly in Dimensions.fs

    let inchPattern = @"\bin(?:ches|ch)?\.?\b"
    let footPattern = @"\b(?:foot|feet|ft)\.?"
    let poundPattern = @"\b(?:pound|lb)s?\.?"
    let cubicFeetPattern = @"\bcu(?:bic)?\.?\s+(?:foot|feet|ft)\.?"
    let gallonPattern = @"\bgal(?:lon)?s?\b"

    let patterns = [
        @"(\d+)\s*(?:x|by)\s*", "$1 x "
        inchPattern, "in."
        footPattern, "ft."
        poundPattern, "lb."
        cubicFeetPattern, "cu.ft."
        gallonPattern, "gal."
        @"(\d+)'", "$1 ft."
        "(\\d+)\"", "$1 in." ]

    let replacers = 
        patterns 
        |> List.map (fun (p,r) -> 
            Regex(p, RegexOptions.IgnoreCase ||| RegexOptions.Compiled), r)

    let measurementRegex =
        Regex(@"\b(?<Whole>\d{1,2})([-\s](?<Numerator>\d{1,2})/(?<Denominator>\d{1,2}))?(?:\s*(in|ft|lb|cu\.ft|gal)\.)?", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

    let inline replace (regex : Regex) (repl : string) (str : string) = regex.Replace(str, repl)

    let collapseMeasurement = 
        Regex(@"(\d)\s(in|ft|lb|cu\.\sft|gal)", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    let standardizeMeasures str =
        let standard = replacers |> List.fold (fun s (p,r) -> p.Replace(s, r)) str
        collapseMeasurement.Replace(standard, "$1$2")


    // given a requested value and the result
    // return a value of 1.0 for a perfect match,
    // - 1.0 for a bad match
//    let closeness (tolerance:float) requested result =
//        1. - 2. * (abs (requested - result) / ((1.0 - tolerance) * max requested result))

    let closeness (tolerance:float) requested result =
        let delta = abs (requested - result) / requested
        if delta < 0.25 then 1.
        elif delta < 0.5 then 0.
        else -1.

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

    let pSimpleFraction : Parser<float,unit> =
        tuple2
            (pint32 .>> spaces .>> pstring "/" .>> spaces)
            pint32
        |>> fun (x,y) -> float x / float y

    let pComplexFraction : Parser<float,unit> =
        tuple3
            (pint32 .>> spaceOrDash)
            (pint32 .>> spaces .>> pstring "/" .>> spaces)
            pint32
        |>> fun (x,y,z) -> float x + float y / float z

    let pMeasure : Parser<float,unit> = pComplexFraction <|> pSimpleFraction <|> pfloat
        

    // basic units / no fractions

    let pVolts : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "volt" <|> (pstring "V" .>> (eof <|> spaces1)))
    let pWatts : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "watt" <|> (pstring "W" .>> (eof <|> spaces1)))
    let pAmps : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "amp")
    let pGallons : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "gal")
    let pPounds : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "lb" <|> pstringCI "pound")
    let pInches : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "inches" <|> pstringCI "in")

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
