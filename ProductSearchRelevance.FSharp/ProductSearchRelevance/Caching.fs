namespace HomeDepot

module Caching =

    open System.IO
    open System.Security.Cryptography
    open FSharp.Data
    open HomeDepot.Utilities

    [<Literal>]
    let data = __SOURCE_DIRECTORY__ + @"..\..\data\"

    [<Literal>] 
    let cache = __SOURCE_DIRECTORY__ + @"..\..\cache\"

    [<Literal>]
    let trainPath = data + "train.csv"
    
    [<Literal>]
    let testPath = data + "test.csv"
    
    [<Literal>]
    let attributesPath = data + "attributes.csv"
    
    [<Literal>]
    let productsPath = data + "product_descriptions.csv"

    type SourceTrain = CsvProvider<trainPath,Schema=",,,,float">
    type SourceTest = CsvProvider<testPath>
    type SourceAttributes = CsvProvider<attributesPath>
    type SourceDescriptions = CsvProvider<productsPath>

    let utilities = __SOURCE_DIRECTORY__ + @"\Utilities.fs"
    
    let fingerPrint (path:string) =

        let fileInfo = FileInfo(path)

        let hasher = MD5.Create()

        let stream = fileInfo.Open(FileMode.Open)
        stream.Position <- 0L
        let hash = hasher.ComputeHash(stream)
        stream.Close ()

        hash |> Array.map string |> String.concat "-"

    let wrapString (txt:string) = sprintf "\"%s\"" txt

    let preprocessTest (row:SourceTest.Row) =
        sprintf "%i,%i,%s,%s"
            row.Id 
            row.Product_uid 
            (row.Product_title |> preprocess |> wrapString)
            (row.Search_term |> preprocess |> cleanMisspellings |> cleanSpaces|> wrapString)

    let preprocessTrain (row:SourceTrain.Row) =
        sprintf "%i,%i,%s,%s,%.2f"               
            row.Id 
            row.Product_uid 
            (row.Product_title |> preprocess |> wrapString)
            (row.Search_term |> preprocess |> cleanMisspellings |> cleanSpaces|> wrapString)
            row.Relevance

    let preprocessAttributes (row:SourceAttributes.Row) =
        sprintf "%i,%s,%s"               
            row.Product_uid 
            (row.Name |> preprocess |> wrapString)
            (row.Value |> preprocess |> wrapString)

    let preprocessDescriptions (row:SourceDescriptions.Row) =
        sprintf "%i,%s"               
            row.Product_uid 
            (row.Product_description |> descriptionSentenceBreak |> preprocess |> wrapString)

    let cacheTrain () =

        printfn "Refreshing cached train"

        let sample = SourceTrain.GetSample()
        let headers = sample.Headers |> Option.get

        let lines = seq { 
            yield headers |> Array.map (sprintf "\"%s\"") |> String.concat ","
            yield! sample.Rows |> Array.ofSeq |> Array.Parallel.map preprocessTrain
            }

        let path = Path.Combine (cache, "train.csv")
        printfn "%s" path
        File.WriteAllLines(path, lines)

    let cacheTest () =

        printfn "Refreshing cached test"

        let sample = SourceTest.GetSample()
        let headers = sample.Headers |> Option.get

        let lines = seq { 
            yield headers |> Array.map (sprintf "\"%s\"") |> String.concat ","
            yield! sample.Rows |> Array.ofSeq |> Array.Parallel.map preprocessTest
            }

        let path = Path.Combine (cache, "test.csv")
        printfn "%s" path
        File.WriteAllLines(path, lines)

    let cacheAttributes () =

        printfn "Refreshing cached attributes"

        let sample = SourceAttributes.GetSample()
        let headers = sample.Headers |> Option.get

        let lines = seq { 
            yield headers |> Array.map (sprintf "\"%s\"") |> String.concat ","
            yield! sample.Rows |> Array.ofSeq |> Array.Parallel.map preprocessAttributes
            }

        let path = Path.Combine (cache, "attributes.csv")
        printfn "%s" path
        File.WriteAllLines(path, lines)

    let cacheDescriptions () =

        printfn "Refreshing cached product descriptions"

        let sample = SourceDescriptions.GetSample()
        let headers = sample.Headers |> Option.get

        let lines = seq { 
            yield headers |> Array.map (sprintf "\"%s\"") |> String.concat ","
            yield! sample.Rows |> Array.ofSeq |> Array.Parallel.map preprocessDescriptions
            }

        let path = Path.Combine (cache, "product_descriptions.csv")
        printfn "%s" path
        File.WriteAllLines(path, lines)

    let invalidateCache () =

        let isInvalid = 
            let expected = fingerPrint utilities
            Directory.EnumerateFiles(cache)
            |> Seq.filter (fun name -> FileInfo(name).Name = expected)
            |> Seq.isEmpty
        
        if isInvalid
        then
            // clear the cache
            Directory.EnumerateFiles(cache)
            |> Seq.iter (File.Delete)

            cacheTrain ()
            cacheTest ()
            cacheAttributes ()
            cacheDescriptions ()

            let stamp = fingerPrint utilities
            let stampPath = cache + stamp
            File.WriteAllText(stampPath,"stamp")

            printfn "Cache refreshed"
        else
            printfn "Cache is up to date"
