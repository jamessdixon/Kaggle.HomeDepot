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

    let inline cleanHtml (txt:string) = WebUtility.HtmlDecode txt
    let inline lowerCase (txt:string) = txt.ToLowerInvariant()
    
    let uniques (words:string[]) = words |> Set.ofArray

    let thousandsSeparator = Regex(@"(\d+),(?=\d{3}(\D|$))", RegexOptions.Compiled)
    let inline cleanThousands (txt:string) =
        thousandsSeparator.Replace(txt,"$1")

    // strip unlikely characters at the ends of a word split by whitespace
    let cleanWordBoundaries (text:string) = 
        Regex.Replace(text, """[("]*(\w+)[.,;?!)"]*""", "$1", RegexOptions.Compiled)

    // extracting values from measure attributes
    let numberExtractor = Regex """(\d+.\d+|.d+|\d+)"""    
    let inline measureOf (text:string) = 
        cleanThousands text
        |> numberExtractor.Match
        |> fun x -> x.Value |> float

    (*
    Tokenization
    *)
    
    type Tokenizer = string -> string[]

    let matchWords = Regex(@"\w+",RegexOptions.Compiled)

    let wordTokenizer : Tokenizer = fun text ->
        text
        |> matchWords.Matches
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> Array.ofSeq
    
    let whiteSpaceTokenizer : Tokenizer = fun text ->
        Regex.Split(text,@"\s+")

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

    let pFraction : Parser<float,unit> =
        tuple3
            (pint32 .>> spaceOrDash)
            (pint32 .>> spaces .>> pstring "/" .>> spaces)
            pint32
        |>> fun (x,y,z) -> float x + float y / float z

    // basic units / no fractions

    let pVolts : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "volt" <|> (pstring "V" .>> (eof <|> spaces1)))
    let pWatts : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "watt" <|> (pstring "W" .>> (eof <|> spaces1)))
    let pAmps : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "amp")
    let pGallons : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "gal")
    let pPounds : Parser<float,unit> = pfloat .>> spaceOrDash .>> (pstringCI "lb" <|> pstringCI "pound")

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
