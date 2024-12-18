#' @keywords internal
# Calculate CAI using sliding windows akin to %MinMax
calculateWindowedCAI <-
  function(CAIFreqDict, codonSeqList, windowSize) {
    caiValues <- numeric()  # Initialize an empty numeric vector
    for (i in 1:(windowSize/2)) {
      caiValues <- c(caiValues, 0)
    }
    for (i in 1:length(codonSeqList)) {
      if (i + windowSize <= length(codonSeqList)) {
        windowValues <- sapply(codonSeqList[i:(i + windowSize - 1)], function(codon) CAIFreqDict[[codon]])
        caiValues <- c(caiValues, geoMean(windowValues))
      }
    }
    if (windowSize %% 2 == 1) {
      for (i in 1:(windowSize/2)) {
        caiValues <- c(caiValues, 0)
      }
    } else {
      for (i in 1:(windowSize/2 - 1)) {
        caiValues <- c(caiValues, 0)
      }
    }
    return(caiValues)
  }
