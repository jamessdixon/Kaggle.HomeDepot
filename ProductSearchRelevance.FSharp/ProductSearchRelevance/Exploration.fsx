#load "Dependencies.fsx"

open HomeDepot.Model
open HomeDepot.Features
open HomeDepot.Utilities

let brands =
    trainset
    |> Seq.choose (fun (_,x) -> x.Product.Attributes.TryFind brandAttribute)
    |> Seq.distinct
    |> Seq.toArray

brands |> Array.filter (fun x-> x.Split ' ' |> Array.length > 1)

trainset
|> Seq.filter (fun (_,x)-> x.SearchTerm.Contains " x ")
//|> Seq.filter (fun (l,o) -> o.SearchTerm |> whiteSpaceTokenizer |> Array.filter (fun x -> x = "x") |> Array.length >= 1)
|> Seq.map (fun (l,o) -> l, o.SearchTerm, o.Product.Title)
|> Seq.take 50
|> Seq.toList
|> Seq.length

attributes
|> Seq.iter (fun kv -> kv.Key |> printfn "%s")

trainset
|> Seq.collect (fun (l,x) -> x.Product.Attributes)
|> Seq.map (fun kv -> kv.Key)
|> Seq.countBy id
|> Seq.sortByDescending snd
|> Seq.toList

trainset
|> Seq.filter (fun (l,o) -> o.SearchTerm |> whiteSpaceTokenizer |> Array.filter (fun x -> x = "x") |> Array.length > 1)
|> Seq.map (fun (l,o) -> l, o.SearchTerm, o.Product.Attributes |> Map.filter (fun k v -> k.Contains "height" || k.Contains "width" || k.Contains "depth") |> Seq.length)
|> Seq.sortBy (fun (x,_,_) -> x)
|> Seq.toList
|> Seq.iter (fun (x,_,y) -> printfn "%f,%i" x y)

trainset
|> Seq.filter (fun (l,o) -> o.SearchTerm |> whiteSpaceTokenizer |> Array.contains ("x"))
|> Seq.map (fun (l,o) -> l, o.Product.Attributes |> Map.filter (fun k v -> k.Contains "height" || k.Contains "width") |> Seq.length)
|> Seq.groupBy fst
|> Seq.map (fun (x,group) -> x, group |> Seq.averageBy (snd >> float))
|> Seq.sortBy fst
|> Seq.toList


|> Seq.sortBy (fun (x,_,_) -> x)
|> Seq.toList
|> Seq.iter (fun (x,_,y) -> printfn "%f,%i" x y)