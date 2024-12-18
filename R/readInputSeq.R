#' @keywords internal
# Go to the file and read the input sequence. This function looks for seqeuences in FASTA format. If there are multiple sequences in the file then only the first sequence is read
readInputSeq <- function(pathToSeqFile) {
  seqFile <- readLines(pathToSeqFile)
  stringSeq <- ""
  seqName <- substr(seqFile[1], 2, nchar(seqFile[1]))  # Extract sequence name
  for (lineNum in 2:length(seqFile)) {
    line <- seqFile[lineNum]
    if (substr(line, 1, 1) == ">") {
      break
    } else {
      stringSeq <- paste0(stringSeq, substr(line, 1, nchar(line)))
    }
  }
  stringSeq <- toupper(stringSeq)
  nucleotideCounts <- table(strsplit(stringSeq, NULL))
  As <- nucleotideCounts["A"]
  Ts <- nucleotideCounts["T"]
  Cs <- nucleotideCounts["C"]
  Gs <- nucleotideCounts["G"]
  if (As + Ts + Cs + Gs != nchar(stringSeq)) {
    cat("ERROR: An unknown character was found in your sequence.\n")
    cat("ERROR: Please rerun the script inputting a sequence only containing A, T, C, and G.\n")
    quit("no", status = 1, runLast = FALSE)
  } else if (nchar(stringSeq) %% 3 != 0) {
    cat("ERROR: The input sequence is not of a length divisible by 3.\n")
    cat("ERROR: Please rerun the script inputting a sequence with a length that is divisible by 3.\n")
    quit("no", status = 1, runLast = FALSE)
  } else {
    return(list(stringSeq = stringSeq, seqName = seqName))
  }
}
