module MirbaseParser

open System
open Mirbase
open FParsec
open Mirbase


let wordParser: Parser<string, unit> =
    spaces
    >>. many1Satisfy isLetter
    .>> spaces

let integerParser: Parser<string, unit> =
    spaces
    >>. many1Satisfy isDigit
    .>> spaces

let skipXX xx : Parser<unit, unit> =     
    skipString xx 
    >>. skipRestOfLine true

let idparser: Parser<string, unit> = 
    skipString "ID" 
    >>. spaces 
    >>. (many1Satisfy2 isLetter (fun c -> isDigit c || isLetter c || c = '-')) 
    .>> skipRestOfLine true
    .>> skipXX "XX"

let primaryAccessionParser: Parser<string, unit> =
    skipString "AC"
    >>. spaces
    >>. (many1Satisfy2 isLetter (fun c -> isDigit c || isLetter c)) 
    .>> skipRestOfLine true
    .>> skipXX "XX"

let speciesParser: Parser<string, unit> =
    skipString "DE"
    >>. spaces
    >>. pipe2 wordParser wordParser (fun a b -> a + " " + b)
    .>> skipRestOfLine true
    .>> skipXX "XX"

let pmidParser: Parser<string, unit> =
    skipString "RX"
    >>. spaces
    >>. skipMany wordParser
    >>. skipString ";"
    >>. spaces
    >>. integerParser
    .>> skipRestOfLine true

let authorsParser: Parser<string list, unit> =
    let authorsLineParser =
        skipString "RA"
        >>. spaces
        >>. restOfLine true

    (many1 authorsLineParser) 
    |>> fun xs -> 
        xs |> String.concat " " 
        |> fun x -> x.Split(',') |> Seq.map (fun x -> x.Trim()) |> Seq.toList

let titleParser: Parser<string, unit> =
    let titleLine =
        skipString "RT"
        >>. spaces
        >>. restOfLine true

    many1 titleLine
    |>> (fun xs -> xs |> String.concat " ")
    

let referenceParser: Parser<string, unit> =
    skipString "RL"
    >>. spaces
    >>. restOfLine true

let descriptionParser: Parser<unit list, unit> =
    choice [
        skipXX "DR";
        skipXX "XX";
        skipXX "CC"
        ]
    |> many
    

let derivateParser: Parser<DerivateTranscript, unit> =
    let skipFT =
        skipString "FT"
        >>? spaces
    let makeDerivative p1 p2 p3 p4 (p5: string): DerivateTranscript = {
        Accession = p1
        Product = p2
        Evidence = p3 |> Evidence.FromString
        Experiment = p4
        Similarity = (if (p5.Length > 0) then Some p5 else None) }
    let accession: Parser<string, unit> = 
        skipFT
        >>? pstring "/accession="
        >>? between (pstring "\"") (pstring "\"") (manySatisfy (fun c -> c <> ' ' && c <> '\"'))
        .>> skipRestOfLine true

    let product: Parser<string, unit> = 
        skipFT
        >>? pstring "/product="
        >>? between (pstring "\"") (pstring "\"") (manySatisfy (fun c -> c <> ' ' && c <> '\"'))
        .>> skipRestOfLine true

    let evidence = 
        skipFT
        >>? pstring "/evidence="
        >>? (manySatisfy (fun c -> c <> '\n'))
        .>> skipRestOfLine true

    let similarity: Parser<string, unit> = 
        skipFT
        >>? pstring "/similarity="
        >>? between (pstring "\"") (pstring "\"") (manySatisfy (fun c -> c <> ' ' && c <> '\"'))
        .>> skipRestOfLine true
        <|> preturn ""
    
    let experiment: Parser<string, unit> =
        printfn "%A" "In Experiment"
        let multilineExperiment =
            printfn "%A" "Multiline experiment"
            restOfLine true
            .>>. manyTill (
                skipString "FT"
                >>? spaces
                >>? (manySatisfy (fun c -> c <> '\n' && c <> '\"'))) (pstring "\"")
            |>> fun (firstLine, otherLines) -> firstLine::otherLines |> String.concat " "
            >>? between (pstring "\"") (pstring "\"") (manySatisfy (fun c -> c <> '\"'))
        
        skipFT
        >>? pstring "/experiment="
        >>? between (pstring "\"") (pstring "\"") (manySatisfy (fun c -> c <> '\"'))
        .>> skipRestOfLine true
        //<|> preturn ""
        <|> multilineExperiment
        <|> preturn ""
    
    pipe5 accession product evidence experiment similarity makeDerivative   
    

let derivatesParser: Parser<DerivateTranscript list, unit> = 
    choice [skipXX "FH"; skipXX "FT   modified_base"; skipXX "FT                   /mod_base"]
    |> many
    >>? (skipXX "FT" >>? derivateParser) |> many1
    .>> skipXX "XX"
    .>> (skipXX "SQ" |> many)
    .>> (skipXX "  " |> many)

let publicationParser: Parser<Publication, unit> = 
    let refun pmid (authors: string list) title reference : Publication = 
        let pmid_int = 
            match pmid with
            | [] -> -1
            | xs -> xs |> List.rev |> List.head |> fun x -> int x
        { 
        Authors = authors
        Title = title
        PMID = pmid_int
        Reference = reference }
    
    pipe4 (pmidParser |> many) authorsParser titleParser referenceParser refun 
    
let publicationsParser = 
    many (skipXX "RN" >>. publicationParser .>> (choice [skipXX "XX"; skipXX "RC"] |> many))


let primaryTranscriptParser =
    let primTranscript id accession species bibl derivates : PrimaryTranscript = {
        Id = id
        Accession = accession
        Species = species
        Publications = bibl
        Derivates = derivates
        GeneRefences = []}

    pipe5 idparser primaryAccessionParser speciesParser (publicationsParser .>> descriptionParser) derivatesParser primTranscript

let mirbaseParser =
    (primaryTranscriptParser
    .>> skipXX "//")
    |> many 

