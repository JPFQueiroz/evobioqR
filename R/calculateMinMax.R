#' @keywords internal
# This function calculates %MinMax values for a given sequence, returned as a list of floats
calculateMinMax <-
  function(sequence, aaFreqDict, freqDict, mapDict, windowSize) {
    minMaxValues <- numeric()
    for (i in 1:(windowSize/2)) {
      minMaxValues <- c(minMaxValues, 0)
    }
    # Using the specified sliding window size (windowSize/2 - 1 on either side of the central codon), min/max is calculated
    for (i in 1:(length(sequence) - windowSize + 1)) {
      window <- sequence[i:(i + windowSize - 1)]
      Actual <- 0.0 # average of the actual codon frequencies
      Max <- 0.0 # average of the min codon frequencies
      Min <- 0.0 # average of the max codon frequencies
      Avg <- 0.0 # average of the averages of all the frequencies associated with each amino acid
      # Sum the frequencies
      for (codon in window) {
        frequencies <- aaFreqDict[[mapDict[[codon]]]] # list of all frequencies associated with the amino acid this codon encodes
        Actual <- Actual + freqDict[[codon]]
        Max <- Max + max(frequencies)
        Min <- Min + min(frequencies)
        Avg <- Avg + sum(frequencies) / length(frequencies)
      }
      # Divide by the window size to get the averages
      Actual <- Actual / windowSize
      Max <- Max / windowSize
      Min <- Min / windowSize
      Avg <- Avg / windowSize
      percentMax <- ((Actual - Avg) / (Max - Avg)) * 100
      percentMin <- ((Avg - Actual) / (Avg - Min)) * 100
      if (percentMax >= 0) {
        minMaxValues <- c(minMaxValues, round(percentMax, 2))
      } else {
        minMaxValues <- c(minMaxValues, round(-percentMin, 2))
      }
    }
    # fills in values for codons where window size makes min/max unable to be calculated
    if (windowSize %% 2 == 1) {
      for (i in 1:(windowSize/2)) {
        minMaxValues <- c(minMaxValues, 0)
      }
    } else {
      for (i in 1:(windowSize/2 - 1)) {
        minMaxValues <- c(minMaxValues, 0)
      }
    }
    return(minMaxValues)
  }
