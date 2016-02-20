namespace HomeDepot

module StringUtils =

    open FuzzyString
    open Iveonik.Stemmers
    open System
    open System.Collections.Concurrent
    open System.Text.RegularExpressions

    let inline splitOnSpaces (str : string) = str.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    let inline containedIn (input : string) (word : string) = input.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0
    let inline wordSimilarity a b = ComparisonMetrics.JaroWinklerDistance(a, b)
    let inline join (strings : string []) = String.Join(" ", strings)
    let inline replace (regex : Regex) (repl : string) (str : string) = regex.Replace(str, repl)
    let inline trim (str : string) = str.Trim()

    /// True if input is similar to word.
    let isFuzzyMatch input word = 
      let m = wordSimilarity input word >= 0.9
      if m then printfn "Fuzzy match '%s' -> '%s'" word input
      m

    /// True if any word in input starts with word.
    let startsWithMatch input word = 
      let word' = Regex.Escape word
      Regex.IsMatch(input, sprintf @"\b%s" word', RegexOptions.IgnoreCase)

    /// True if any word in input equals word.
    let wordMatch input word = 
      let word' = Regex.Escape word
      Regex.IsMatch(input, sprintf @"\b%s\b" word', RegexOptions.IgnoreCase)

    let stemDict = ConcurrentDictionary<string, string>(StringComparer.OrdinalIgnoreCase)
    let inline stem word = stemDict.GetOrAdd(word, (fun s -> (new EnglishStemmer()).Stem s))

    let stemWords = 
      splitOnSpaces
      >> Array.map stem
      >> String.concat " "

    let stopWords = 
      [| "and"; "the"; "to"; "for"; "a"; "with"; "of"; "is"; "or"; "your"; "this"; "from"; "on"; "that"; "easy"; "are"; "be"; "it"; "an"; "you"; 
         "use"; "can"; "by"; "up"; "design"; "features"; "as"; "any"; "has"; "provides"; "not"; "will"; "installation"; "residents"; "designed"; 
         "see"; "proposition"; "nbsp"; "at"; "used"; "provide"; "more"; "may"; "when"; "offers"; "construction"; "product"; "allows"; "other"; 
         "made"; "no"; "includes"; "most"; "perfect"; "durable"; "depot"; "also"; "easily"; "our"; "its"; "included"; "warranty"; "than"; "help"; 
         "look"; "per"; "plan"; "into"; "one"; "while"; "these"; "limited" |]
    let stopRegex = Regex(sprintf @"\b(?:%s)\b" <| String.concat "|" stopWords, RegexOptions.Compiled)
    let inline removeStopWords (str : string) = stopRegex.Replace(str, String.Empty)

    let sanitize (str : string) = 
      let clean = System.Text.StringBuilder(str.Length)
      for char in str do
        match char with
        | c when Char.IsLetterOrDigit c || Char.IsWhiteSpace c -> clean.Append char |> ignore
        | '-' -> clean.Append " " |> ignore
        | _ -> ()
      clean.ToString().TrimEnd()

    let inline toLower (str : string) = str.ToLowerInvariant()

    let prepareText str = 
      str
      |> toLower
      |> splitOnSpaces
      |> Array.map sanitize
      |> Array.collect splitOnSpaces
      |> Array.map stem
