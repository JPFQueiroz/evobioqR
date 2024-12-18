#' @keywords internal
# Takes in an mRNA sequence as a string and returns it as a list with each element in the list being a specific codon
generateCodonSeq <- function(stringseq) {
  codonSeq <- character() # list of codons to be returned
  extras <- ""
  for (line in stringseq) {
    line <- trimws(line)
    string <- paste0(extras, line)
    i <- 1
    j <- 3
    while (j <= nchar(string)) {
      codonSeq <- c(codonSeq, substr(string, i, j))
      i <- i + 3
      j <- j + 3
    }
    extras <- substr(string, i, nchar(string))
  }
  return(codonSeq)
}
