#I @"../packages/"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"
#load "CsvData.fs"
#load "Dimensions.fs"

open HomeDepot

// NOTE the train target variable is an average of at least 3 human raters. It could be more than 3.

let distribution = 
    CsvData.train.Rows
    |> Seq.countBy (fun r -> r.Relevance)
    |> Seq.sortBy fst
    |> Seq.iter (fun (r,c) -> printfn "%A:%A" r c )

// is the sample reasonably evenly distributed?
// checking to see how smart we need to be for cross-validation

let size = CsvData.train.Rows |> Seq.length
CsvData.train.Rows |> Seq.take (size/2) |> Seq.averageBy (fun x -> x.Relevance)  
CsvData.train.Rows |> Seq.skip (size/2) |> Seq.averageBy (fun x -> x.Relevance)             
           
let trainIds = CsvData.train.Rows |> Seq.map (fun r -> r.Product_uid) |> Set.ofSeq // 54667 distinct products in train
let testIds = CsvData.test.Rows |> Seq.map (fun r -> r.Product_uid) |> Set.ofSeq // 97460 distinct products in test
let productIds = Set.union trainIds testIds // 124428 distinct products in train + test
let overlap = Set.intersect trainIds testIds // 27699 products appear in both train & test
let trainIdsOnly = Set.difference trainIds testIds // 26968 products appear only in train
let testIdsOnly = Set.difference testIds trainIds // 69761 products appear only in test

let attributes = CsvData.getAttributeMap() // 86263 products with attributes

let attributeCount = CsvData.attributes.Rows |> Seq.length // 2044648 attributes
let attributeKeys =
  CsvData.attributes.Rows
  |> Seq.map (fun r -> r.Name.ToLowerInvariant())
  |> Seq.distinct
  |> Seq.sort // 5343 distinct attribute names
let attributeValues =
  CsvData.attributes.Rows
  |> Seq.map (fun r -> r.Value.ToLowerInvariant())
  |> Seq.distinct
  |> Seq.sort // 301744 distinct attribute values

let trainQueries = CsvData.train.Rows |> Seq.map (fun r -> r.Search_term.ToLowerInvariant()) |> Set.ofSeq
let testQueries = CsvData.test.Rows |> Seq.map (fun r -> r.Search_term.ToLowerInvariant()) |> Set.ofSeq
let queries = Set.union trainQueries testQueries // 24601 distinct queries in train + test

// look for queries with dimensions
open Dimensions
open System.Text.RegularExpressions
let dimQueries = queries |> Seq.collect (standardizeMeasures >> measurementRegex.Matches >> Seq.cast<Match>)

let trainTitles = CsvData.train.Rows |> Seq.map (fun r -> r.Product_title, r.Search_term)
let lastWord (s:string) = s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.last
let sameEnds = trainTitles |> Seq.filter (fun (t,s) -> lastWord t = lastWord s)

#load "Dependencies.fsx"
open HomeDepot.Model
open HomeDepot.Utilities
open FSharp.Data

[<Literal>]
let rawAttributesPath = @"../data/attributes.csv"
type RawAttributes = CsvProvider<rawAttributesPath>

[<Literal>]
let trainPath = @"../data/train.csv"
[<Literal>]
let testPath = @"../data/test.csv"
[<Literal>]
let attributesPath = @"../data/attributes.csv"
[<Literal>]
let productsPath =  @"..\data\product_descriptions.csv"

[<Literal>]
let submissionPath =  @"../data/"

type Train = CsvProvider<trainPath,Schema=",,,,float">
type Test = CsvProvider<testPath>
type AllAttributes = CsvProvider<attributesPath>
type AllProducts = CsvProvider<productsPath>

let brandNames = 
    AllAttributes.GetSample().Rows
    |> Seq.filter (fun x -> x.Name = "MFG Brand Name")
    |> Seq.map (fun x -> x.Value |> preprocess)
    |> Seq.distinct
    |> Seq.toList

brandNames.Length

brandNames |> List.filter (fun x -> x.Contains "k")

Train.GetSample().Rows 
|> Seq.map (fun x -> x.Search_term.ToLowerInvariant())
//|> Seq.iter (fun x -> printfn "%s" x)
|> Seq.filter (fun x -> x.Contains "batting") |> Seq.toList

Train.GetSample().Rows 
|> Seq.map (fun x -> x.Product_title.ToLowerInvariant())
|> Seq.filter (fun x -> x.Contains "multi tool") |> Seq.toList

let foo = Regex(@"\bporcel[a-z]*\b")
let bar (txt:string) = foo.Replace (txt,"everbilt")

brandNames |> Seq.find (fun x -> x.Contains "vig")

let titlesVocabulary = 
    Train.GetSample().Rows
    |> Seq.distinctBy (fun x -> x.Product_uid)
    |> Seq.map (fun x -> 
        x.Product_title 
        |> preprocess 
        |> whiteSpaceTokenizer 
        |> Set.ofArray)
    |> Set.unionMany 

let brandsVocabulary = 
    brandNames
    |> Seq.map (whiteSpaceTokenizer >> uniques)
    |> Set.unionMany
       
let searchVocabulary = 
    Train.GetSample().Rows
    |> Seq.distinctBy (fun x -> x.Search_term)
    |> Seq.map (fun x -> x.Search_term |> preprocess)
    |> Seq.collect whiteSpaceTokenizer
    |> Seq.countBy id    
    |> Seq.toArray

let misspell =     
    searchVocabulary
    |> Array.filter (fun (word,count) -> not (titlesVocabulary.Contains word))
    |> Array.filter (fun (word,count) -> not (brandsVocabulary.Contains word))

misspell.Length

misspell 
|> Array.filter (fun x -> snd x > 1) 
|> Array.iter (fun x -> printfn """    "%s"," " """ (fst x))

let descriptionVocabulary = 
    AllProducts.GetSample().Rows
    |> Seq.map (fun x -> x.Product_description |> preprocess)
    |> Seq.collect whiteSpaceTokenizer
    |> Seq.countBy id    
    |> Seq.toArray

ryobl
//hinghs
//chanpayne
//incide
Train.GetSample().Rows 
|> Seq.take 100 
|> Seq.map (fun x -> x.Product_title, x.Product_title |> preprocess) |> Seq.toList

Train.GetSample().Rows 
|> Seq.collect (fun x -> x.Search_term |> preprocess |> whiteSpaceTokenizer |> uniques)
|> Seq.countBy id
|> Seq.sortBy snd
|> Seq.skip 1000
|> Seq.take 100
|> Seq.toList

AllAttributes.GetSample().Rows
|> Seq.filter (fun x -> x.Name.Contains "Product Type")
|> Seq.groupBy (fun x -> x.Name)

trainset
|> Seq.filter (fun (a,b) -> b.SearchTerm.Contains "pounds")

Train.GetSample().Rows 
|> Seq.map (fun x -> x.Search_term, x.Search_term |> preprocess)
|> Seq.take 50
|> Seq.toList

let types = 
    attributes 
    |> Map.filter (fun key value -> key.EndsWith "product type")
    |> Map.map (fun key values ->
        let words =
            values 
            |> Seq.collect whiteSpaceTokenizer
            |> Seq.countBy id
        let largest = float (words |> Seq.map snd |> Seq.max)
        words
        |> Seq.map (fun (w,c) -> w, float c / largest)
        |> Map.ofSeq)

types |> Map.map (fun k v -> v |> Seq.toList)

let binaries = 
    attributes 
    |> Map.filter (fun key value -> value.Contains "no")
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.distinct 


trainset 
|> Seq.map (fun (a,b) -> b.SearchTerm)
|> Seq.filter (fun b -> b.Contains " zero ")

trainset
|> Seq.map (fun (a,b) -> 
    b.Product.Attributes 
    |> Seq.filter (fun kv -> kv.Value = "no")
    |> Seq.map (fun kv -> kv.Key))

let test = 
    trainset
    |> Seq.groupBy (fun (a,b) -> b.SearchTerm)
    |> Seq.take 10
    |> Seq.map (fun (s,xs) -> 
        s, 
        xs 
        |> Seq.sortBy fst 
        |> Seq.map (fun (a,b) -> a, b.Product.Title)
        |> Seq.toList)
    |> Seq.toList



trainset
|> Seq.map (fun (a,b) ->
    b.Product.Title)
|> Seq.filter (fun x -> x.Contains ".")
|> Seq.truncate 500
|> Seq.toList
let r = Regex("(cu\.|cubic)\s*ft\.")
let test (t:string) = r.Replace(t,"cubicfeet")
test "3 cu ft"

Train.GetSample().Rows 
|> Seq.filter (fun x -> x.Search_term.Contains "sq.")
|> Seq.map (fun x -> x.Search_term |> test)
|> Seq.take 20
|> Seq.toList

Train.GetSample().Rows 
|> Seq.filter (fun x -> x.Search_term |> preprocess |> fun x -> x.Contains " t ")
|> Seq.iter (fun x -> x.Search_term  |> printfn "%s")

("abc").StartsWith("a")
Train.GetSample().Rows 
|> Seq.take 100 
|> Seq.map (fun x -> x.Product_title, x.Product_title |> preprocess) |> Seq.toList

trainset 
|> Seq.map (fun (_,x) -> x.SearchTerm, x.SearchTerm |> whiteSpaceTokenizer)
|> Seq.take 50
|> Seq.iter (printfn "%A")




trainset |> Seq.take 100 |> Seq.map (fun (_,x) -> x.SearchTerm |> stem) |> Seq.iter (printfn "%s")

trainset |> Seq.length

trainset |> Seq.filter (fun (_,x) -> x.SearchTerm.Contains "one") |> Seq.iter (fun (_,x) -> x.SearchTerm |> printfn "%s")

trainset 
|> Seq.take 100 
|> Seq.map (fun (l,x) -> x.Product.Title)
|> Seq.distinct
|> Seq.iter (printfn "%A")

trainset |> Seq.map (fun (_,x) -> x.Product.Title |> Set.ofSeq) |> Set.unionMany |> Set.iter (printf "%A ")

let searchFor (s:string) =
    trainset 
    |> Seq.filter ((fun (_,x) -> x.Product.Title.Contains s)) 
    |> Seq.take 20 
    |> Seq.iter (fun (_,x) -> printfn "%s" x.Product.Title)
    trainset 
    |> Seq.filter ((fun (_,x) -> x.SearchTerm.Contains s)) 
    |> Seq.take 20 
    |> Seq.iter (fun (_,x) -> printfn "%s" x.SearchTerm)

cleanWordBoundaries """ "this" or-that is.it,right 3.5"""

trainset
|> Seq.map (fun (_,x) -> x.SearchTerm |> lowerCase)
|> Seq.filter (fun x -> x.Contains("door")) 
|> Seq.iter (printfn "%s")
trainset 
|> Seq.map (fun (_,x) -> x.SearchTerm |> wordTokenizer |> Array.map stem)
|> Seq.concat
|> Seq.countBy id
|> Seq.sortBy snd
|> Seq.toList

