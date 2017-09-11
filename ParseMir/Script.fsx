// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r@"..\packages\FParsec\lib\net40-client\FParsecCS.dll"
#r@"..\packages\FParsec\lib\net40-client\FParsec.dll"

#load "Mirbase.fs"
#load "MirbaseParser.fs"
open System
open System.IO
open FParsec
open MirbaseParser

// for more info about mirbase and miRNA/microRNA please check mirbase.org
// download data from ftp://mirbase.org/pub/mirbase/21/miRNA.dat.zip
// unzip it and ensure your path value is correct

let path = "../../Data/miRNA.dat"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let allFile = 
    File.ReadAllText path

test mirbaseParser allFile


