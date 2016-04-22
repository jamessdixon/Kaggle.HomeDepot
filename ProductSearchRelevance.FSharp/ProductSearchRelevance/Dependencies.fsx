(*
Core dependencies

This file is intended to be loaded at the top of scripts, using:
#load "Dependencies.fsx"
Once this is done, the core dependencies are available in the script.
*)

#I __SOURCE_DIRECTORY__
#I "../packages/"

#r @"FSharp.Data/lib/net40/FSharp.Data.dll"
#r @"StemmersNet/lib/net20/StemmersNet.dll"
#r @"FSharp.Collections.ParallelSeq/lib/net40/FSharp.Collections.ParallelSeq.dll"

#r @"C:\Users\Taylor\Documents\Visual Studio 2015\Projects\Word2Vec\Word2Vec.Net\bin\Release\Word2Vec.Net.dll"

#load "Utilities.fs"
#load "Caching.fs"

open HomeDepot.Caching
invalidateCache ()

#load "Model.fsi"
#load "Colors.fs"
#load "Model.fs"
#load "Features.fs"
