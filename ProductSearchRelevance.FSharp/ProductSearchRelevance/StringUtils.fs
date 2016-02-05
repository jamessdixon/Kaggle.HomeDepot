module StringUtils

open FuzzyString
open Iveonik.Stemmers
open System
open System.Collections.Concurrent
open System.Text
open System.Text.RegularExpressions

let inline splitOnSpaces (str : string) = str.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
let inline containedIn (input : string) word = input.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0
let inline wordSimilarity a b = ComparisonMetrics.OverlapCoefficient(a, b)

/// True if any word in input is similar to word.
let isFuzzyMatch input word = 
  let inputWords = splitOnSpaces input
  inputWords |> Array.exists (fun i -> wordSimilarity i word >= 0.7)

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

//let stopWords = [| "and"; "for"; "the"; "w/"; "with" |]
//let removeStopWords (str : string) : string = 
//  let cleaned = stopWords |> Array.fold (fun s w -> Regex.Replace(s, sprintf @"\b%s\b" w, "", RegexOptions.IgnoreCase)) str
//  cleaned.ToString()
let sanitize (str : string) = 
  let clean = StringBuilder(str.Length)
  for char in str do
    match char with
    | c when Char.IsLetterOrDigit c || Char.IsWhiteSpace c -> clean.Append char |> ignore
    | '-' | '/' -> clean.Append " " |> ignore
    | _ -> ()
  clean.ToString().Trim()

let inline toLower (str : string) = str.ToLowerInvariant()

let prepareText = 
  toLower
  >> sanitize
  >> stemWords
