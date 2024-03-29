% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dna2prt.R
\name{convert}
\alias{convert}
\title{DNA/RNA sequence to amino acid sequence}
\usage{
convert(dna)
}
\arguments{
\item{dna}{A character vector containing a single text string of DNA}
}
\value{
A character vector containing a single text string of amino acids
}
\description{
DNA/RNA sequence to amino acid sequence
}
\examples{
# DNA:
dna <- "AtgcTTCggagtcgT"
convert(dna)
# RNA:
rna <- "AuGUCGCAgGuGU"
convert(rna)
}
