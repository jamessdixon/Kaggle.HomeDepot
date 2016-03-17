
//Install-Package NHunspell 

//Put the following files into %appdata%\Local\Temp
//th_en_us_new.dat, en_us.aff, en_us.dic
//Put the following file into C:\Program Files (x86)\Microsoft SDKs\F#\4.0\Framework\v4.0
//Hunspellx64.dll
//I put all of the files into a libs folder

#r "../packages/NHunspell/lib/net/NHunspell.dll"

open NHunspell

let thesaurus = new MyThes("th_en_us_new.dat");
let hunspell = new Hunspell("en_us.aff", "en_us.dic")
let thesaurusEntry = thesaurus.Lookup("cars", hunspell);
thesaurusEntry.Meanings |> Seq.iter(fun m -> m.Synonyms |> Seq.iter(fun s -> printfn "%s" s ))