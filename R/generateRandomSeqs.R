#' @keywords internal
# Generate the desired number of random starting seeds for harmonization
generateRandomSeqs <- function(wtSequence, numHarmonize) {
  seeds <- list()
  for (i in 1:numHarmonize) {
    newSeq <- character(length(wtSequence))
    for (position in seq_along(wtSequence)) {
      aa <- mapDict[[wtSequence[position]]]
      options <- aaDict[[aa]]
      choice <- sample(options, size = 1)
      newSeq[position] <- choice
    }
    seeds[[i]] <- newSeq
  }
  return(seeds)
}
