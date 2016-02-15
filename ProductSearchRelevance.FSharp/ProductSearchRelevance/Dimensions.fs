module Dimensions

open System.Text.RegularExpressions

let inchPattern = @"\bin(?:ches|ch)?\.?\b"
let footPattern = @"\b(?:foot|feet|ft)\.?"
let poundPattern = @"\b(?:pound|lb)s?\.?"
let cubicFeetPattern = @"\bcu(?:bic)?\.?\s+(?:foot|feet|ft)\.?"
let gallonPattern = @"\bgal(?:lon)?s?\b"

let patterns = 
  [ @"(\d+)\s*(?:x|by)\s*", "$1 x "
    inchPattern, "in."
    footPattern, "ft."
    poundPattern, "lb."
    cubicFeetPattern, "cu.ft."
    gallonPattern, "gal."
    @"(\d+)'", "$1 ft."
    "(\\d+)\"", "$1 in." ]

let replacers = patterns |> List.map (fun (p,r) -> Regex(p, RegexOptions.IgnoreCase ||| RegexOptions.Compiled), r)

let measurementRegex =
  Regex(@"\b(?<Whole>\d{1,2})([-\s](?<Numerator>\d{1,2})/(?<Denominator>\d{1,2}))?(?:\s*(in|ft|lb|cu\.ft|gal)\.)?", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let inline replace (regex : Regex) (repl : string) (str : string) = regex.Replace(str, repl)
let collapseMeasurement = Regex(@"(\d)\s(in|ft|lb|cu\.\sft|gal)", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
let standardizeMeasures str =
  let standard = replacers |> List.fold (fun s (p,r) -> p.Replace(s, r)) str
  collapseMeasurement.Replace(standard, "$1$2")
