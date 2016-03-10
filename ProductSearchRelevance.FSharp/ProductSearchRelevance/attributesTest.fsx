#load "Dependencies.fsx"

open System
open System.IO
open HomeDepot.Model
open HomeDepot.Features

#r "alglibnet2/lib/alglibnet2.dll"

let analyze (sample:Example[]) (features:FeatureLearner[]) =
   
    let features = 
        featurizer features (sample |> Array.map snd)

    let trainInputOutput =
        sample
        |> Array.Parallel.map (fun (label,observation) ->
            let fs = observation |> extract features
            Array.append fs [| label |])
        |> array2D

    let trees = 500
    let proportionHeldOut = 0.25
    let sampleSize = sample.Length
    let featureCount = features.Length
    let featuresUsed = sqrt (float featureCount) |> ceil |> int

    let _info, forest, forestReport =
        alglib.dfbuildrandomdecisionforestx1(
            trainInputOutput, 
            sampleSize, 
            featureCount, 
            1, 
            trees, 
            featuresUsed, 
            proportionHeldOut)
    
    forestReport.oobrmserror

let filename = "attributes-strength"

let desktop =
    Environment.SpecialFolder.Desktop
    |> Environment.GetFolderPath

let targetFolder = Path.Combine(desktop,"kaggle")

if (not (Directory.Exists targetFolder))
then
    Directory.CreateDirectory(targetFolder)
    |> ignore

let filePath = Path.Combine(targetFolder,filename)

//let attributeNames = attributes |> Seq.map (fun kv -> kv.Key)

let attributeNames = [
   "mfg brand name"
   "product width (in.)"
   "product height (in.)"
   "product depth (in.)"
   "product weight (lb.)"
   "color family"
   "material"
   "color/finish"
   "certifications and listings"
   "assembled height (in.)"
   "assembled width (in.)"
   "assembled depth (in.)"
   "product length (in.)"
   "indoor/outdoor"
   "commercial / residential"
   "energy star certified"
   "package quantity"
   "hardware included"
   "flooring product type"
   "color"
   "tools product type"
   "included"
   "voltage (volts)"
   "assembly required"
   "features"
   "wattage (watts)"
   "finish"
   "shape"
   "color/finish family"
   "electrical product type"
   "finish family"
   "fixture color/finish"
   "product thickness (in.)"
   "style"
   "interior/exterior"
   "number of bulbs required"
   "coverage area (sq. ft.)"
   "finish type"
   "collection name"
   "power tool product type"
   "paint product type"
   "outdoor living product type"
   "hardware finish family"
   "bulb type included"
   "reconditioned"
   "light source"
   "amperage (amps)"
   "paint/stain key features"
   "container size"
   "bulb type"
   "dry to touch (min.)"
   "light bulb base code"
   "fastener type"
   "product thickness (mm)"
   "builders hardware product type"
   "door type"
   "transparency"
   "paint/stain clean up"
   "door handing"
   "sheen"
   "weight capacity (lb.)"
   "adjustable lamp head"
   "frame material"
   "application type"
   "time before recoating (hours)"
   "appliance type"
   "mount type"
   "number of doors"
   "fixture color/finish family"
   "product length (ft.)"
   "maximum wattage (watts)"
   "application method"
   "weather resistant"
   "number of pieces"
   "approximate tile size"
   "accessory type"
   "faucet type"
   "shade color family"
   "size"
   "decor product type"
   "paintable/stainable"
   "connection type"
   "rgb value"
   "power type"
   "flow rate (gallons per minute)"
   "kitchen product type"
   "measurement standard"
   "storage product type"
   "faucet features"
   "mildew resistant"
   "faucet included components"
   "number in package"
   "coating product category"
   "type"
   "glass style"
   "waterproof"
   "door thickness (in.)"
   "paint/stain/waterproofer product type"
   "door size (wxh) in."
   "handle type"
]

attributeNames
|> Seq.map (fun name ->
    let feature : FeatureLearner =
        fun sample ->
            fun obs -> 
                match (obs.Product.Attributes.TryFind name) with
                | None -> 0.
                | Some(_) -> 1.
    name, [| feature |] )
|> Seq.map (fun (name, feature) -> name, analyze trainset feature)
|> Seq.sortByDescending snd
|> Seq.map (fun (name, rmse) -> 
    printfn "%s,%.6f" name rmse
    sprintf "%s,%.6f" name rmse)
|> fun data -> File.WriteAllLines(filePath, data)


