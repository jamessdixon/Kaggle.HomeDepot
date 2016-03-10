#r @"C:\Users\Mathias Brandewinder\Documents\GitHub\Kaggle.HomeDepot\ProductSearchRelevance.FSharp\packages\FParsec\lib\net40-client\FParsecCS.dll"
#r @"C:\Users\Mathias Brandewinder\Documents\GitHub\Kaggle.HomeDepot\ProductSearchRelevance.FSharp\packages\FParsec\lib\net40-client\FParsec.dll"
open FParsec

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

run pMeasure "1/2"

let pfoo : Parser<float,unit> =
    (tuple2 (pint32 .>> pstring ".") pint32) <|> (tuple2 (pint32 .>> pstring "/") pint32 |>> fun (x,y) -> float x / float y)

run pfoo "1/2"

open System.Text.RegularExpressions

let simple = Regex(@"\d*\.*\d+")
let simpleNumber (text:string) = simple.Match text

simpleNumber " 1.2a 3/4"

let bar (text:string) = Regex.Replace(text,@"(\d*\.*\d+)","$1") |> float
bar "123.45"

let fraction = Regex(@"(\d+)\s*\/\s*(\d+)")

let foo (text:string) = Regex.Replace(text,@"(\d+)\s*\/\s*(\d+)","$1./$2.")

let fractionNumber (text:string) = fraction.Match text

fractionNumber "1 2 /  23 x 23 1/2"

let complex = Regex(@"\d+\s\d+\s*\/\s*\d+")
let complexNumber (text:string) = complex.Matches text

complexNumber "1 2 /  23 x 23 1/2"

let test = Regex("^-?(?<WholeNumber>\d+)(?<Partial>(\.(?<Decimal>\d+))|(/(?<Denomiator>\d+))|(\s(?<Fraction>\d+/\d+)))?$")
let m = test.Match "42 12/30"
m.Groups.["WholeNumber"]
m.Groups.["Denomiator"]

test.GetGroupNames()

let bad = """(\d+)\s*([^/]|\w)"""
let realInt = Regex(@"(\d+)[/]")
let x = "12/"

realInt.Matches x

let hell = Regex("""(?<float>\d*\.*\d+)""")

let test = hell.Matches("3.14 0.5 .123 1/2")

test.[3].Groups.["numerator"]

let complicated = Regex(@"(?<float>\d*\.\d+)|(?<integer>\d*)\s+(?<numerator>\d+)\s*\/\s*(?<denominator>\d+)", RegexOptions.Compiled)

let extract (text:string) =
    let results = complicated.Matches(text)
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

