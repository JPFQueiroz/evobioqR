#' @keywords internal
# Do harmonization initialization (Rodriguez Harmonization) by choosing codons of the same rank in each organism
initializeHarmonizedSequence <-
  function(codonSeq, fromFreqDict, toFreqDict, toAAFreqDict, toCAIFreqDict,
           windowSize, method) {
    initSeq <- character()  # Initialize an empty character vector for the harmonized sequence
    for (codon in codonSeq) { # chose codon of equal rank in other species
      numBelow <- 1 ### I changed from 0 to 1 ###
      optionList <- aaDict[[mapDict[[codon]]]]

      for (i in optionList) {
        if (fromFreqDict[[i]] < fromFreqDict[[codon]]) {
          numBelow <- numBelow + 1
        }
      }
      toFreqOptions <- sort(toAAFreqDict[[mapDict[[codon]]]])
      desiredFreq <- toFreqOptions[numBelow]
      flag <- 0  # needed to add a flag because sometimes different codons have the same frequency
      for (i in optionList) {
        if (toFreqDict[[i]] == desiredFreq && flag == 0) {
          initSeq <- c(initSeq, i)
          flag <- 1
        }
      }
    }
    if (method == "MM") {
      initialValues <- calculateMinMax(initSeq, toAAFreqDict, toFreqDict,
                                       mapDict, windowSize)
    } else if (method == "GM") {
      initialValues <- calculateWindowedCAI(toCAIFreqDict, initSeq, windowSize)
    }
    return(list(initSeq = initSeq, initialValues = initialValues))
  }
