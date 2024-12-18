#' @keywords internal
# Takes in a dictionary mapping codons to frequencies and amino acids to codons and returns a dictionary mapping amino acids to a list of their respective codon frequencies
generateAAFreqDict <- function(freqDict, aaDict) {
  aaFreqDict <- list()
  for (aa in names(aaDict)) {
    aaFreqDict[[aa]] <-
      sapply(aaDict[[aa]], function(codon) freqDict[[codon]])
  }
  return(aaFreqDict)
}
