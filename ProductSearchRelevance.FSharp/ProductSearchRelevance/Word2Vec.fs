namespace HomeDepot

open System
open System.IO
open System.Collections.Concurrent

module Word2Vec =

    let w2vPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory), "Kaggle/output.bin")
    let word2vec = Word2Vec.Net.Distance(w2vPath)

    let w2vDict = ConcurrentDictionary<string, (string * float) []>() // better perf during feature extract

    let getCloseWords word =
        w2vDict.GetOrAdd(word, (word2vec.Search >> Array.map (fun bw -> bw.Word, float bw.Distance)))
