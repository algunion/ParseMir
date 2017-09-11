module Mirbase
    
type GeneReference = {
    Nomenclature: string
    OfficialSymbol: string
    GeneId: int
    }
    
type Evidence =
    | Experimental
    | NotExperimental
with
    static member FromString s =
        match s with
        | "experimental" -> Experimental
        | _ -> NotExperimental

type Publication = {
    Authors: string list
    Title: string
    PMID: int
    Reference: string }
    
type DerivateTranscript = {
    Accession: string
    Product: string
    Evidence: Evidence
    Similarity: string option
    Experiment: string}

type PrimaryTranscript = {
    Species: string
    Id: string
    Accession: string
    Publications: Publication list
    Derivates: DerivateTranscript list
    GeneRefences: GeneReference list }
