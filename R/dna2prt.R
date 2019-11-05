#' Convert nucleotide sequence to amino acid sequence
#'
#' @param dna A character vector containing a single text string of DNA
#' @return A character vector containing a single text string of amino acids
#' @export
#'

dna <- "ATGtacacagctaaaGAacggauuuctacacatGAAGgtagagaggccacagagagcacguag"

convert <- function(dna){
  aaTable <- matrix(c("TTT", "TTC", "TTA", "TTG",
                    "CTT", "CTC", "CTA", "CTG",
                    "GTT", "GTC", "GTA", "GTG",
                    "ATT", "ATC", "ATA", "ATG",
                    "TCT", "TCC", "TCA", "TCG",
                    "CCT", "CCC", "CCA", "CCG",
                    "GCT", "GCC", "GCA", "GCG",
                    "ACT", "ACC", "ACA", "ACG",
                    "TAT", "TAC", "TAA", "TAG",
                    "CAT", "CAC", "CAA", "CAG",
                    "GAT", "GAC", "GAA", "GAG",
                    "AAT", "AAC", "AAA", "AAG",
                    "TGT", "TGC", "TGA", "TGG",
                    "CGT", "CGC", "CGA", "CGG",
                    "GGT", "GGC", "GGA", "GGG",
                    "AGT", "AGC", "AGA", "AGG",
                    "F", "F", "L", "L",
                    "L", "L", "L", "L",
                    "V", "V", "V", "V",
                    "I", "I", "I", "M",
                    "S", "S", "S", "S",
                    "P", "P", "P", "P",
                    "A", "A", "A", "A",
                    "T", "T", "T", "T",
                    "Y", "Y", "*", "*",
                    "H", "H", "Q", "Q",
                    "D", "D", "E", "E",
                    "N", "N", "K", "K",
                    "C", "C", "*", "W",
                    "R", "R", "R", "R",
                    "G", "G", "G", "G",
                    "S", "S", "R", "R"), 64, 2)
  DNA <- toupper(dna)
  DNA <- gsub("U", "T", DNA)
  numCodons <- nchar(DNA)/3
  posCodon <- seq(from = 1, by = 3, length.out = numCodons)
  aaVector <- vector(mode = "character", length = numCodons)
  for (i in 1:numCodons){
    currentCodon <- substr(DNA, posCodon[i], posCodon[i]+2)
    #codonVector[i] <- currentCodon
    for (j in 1:length(aaTable[,1])){
      if (currentCodon == aaTable[j,1]) aaVector[i] <- aaTable[j,2]
    }
  }
  aaSeq <- paste0(aaVector, collapse = "")
  return (aaSeq)
}
