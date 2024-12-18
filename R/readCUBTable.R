#' @keywords internal
# This function reads in a CUB table where the format for a line is: <Codon> <Frequency> repeated for each codon.
readCUBTable <- function(path) {
  cubDict <- numeric()  # Initialize an empty numeric vector
  cubTable <- readLines(path)
  for (line in cubTable) {
    line <- gsub("U", "T", line, fixed = TRUE)
    line <- strsplit(line, "\\s+")[[1]]
    if (length(line) == 2) {
      if (line[1] %in% names(mapDict)) {
        cubDict[[line[1]]] <- as.numeric(line[2])
      }
    }
  }
  if (length(cubDict) != length(mapDict)) {
    cat("The following codons are missing/in the incorrect format in your input CUB table:\n")
    missing_codons <- setdiff(names(mapDict), names(cubDict))
    cat(paste(missing_codons, collapse = "\n"), "\n")
    cat("Please fix these and rerun the script.\n")
    q("no", status = 1, runLast = FALSE)  # exit R with an error status
  }
  return(cubDict)
}
