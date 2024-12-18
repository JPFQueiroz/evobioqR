#' @keywords internal
# The main harmonization function - finds areas where there is a lot of error between the target and the current model (harmonized) values, and attempts to make educated codon changes in these regions to decrease the total deviation between the harmonized values and the target values.
harmonizedSeqErrorCorrection <-
  function(codonSeq, harmonizedValues, targetValues, toFreqDict, toAAFreqDict,
           toCAIFreqDict, windowSize, method) {
    iteration <- 0
    flag <- 0
    while (flag < 5) {
      totalDist <- sum(abs(harmonizedValues - targetValues))
      start <- totalDist  # to check at end of iteration if sequence improved or not
      aboveBelow <- integer()  # 0 is below, 1 is above, 2 is equal, tracks for regions of 'continuous error'
      for (i in 1:length(codonSeq)) {
        if (targetValues[i] > harmonizedValues[i]) {
          aboveBelow <- c(aboveBelow, 0)
        } else if (targetValues[i] < harmonizedValues[i]) {
          aboveBelow <- c(aboveBelow, 1)
        } else {
          aboveBelow <- c(aboveBelow, 2)
        }
      }
      last <- -1
      consecutive <- 0
      for (i in 1:length(aboveBelow)) {
        if (aboveBelow[i] == last) {
          consecutive <- consecutive + 1
        } else {
          EC_size <- 10
          if (consecutive >= EC_size && last != 2) {
            codonSeq2 <- codonSeq  # copy of codon sequence for editing
            numReplace <- floor(consecutive / EC_size)  # number of positions in continuous region to change
            replaceStart <- i - consecutive
            replacePos <- vector()
            for (j in 1:numReplace) {
              replacePos <- c(replacePos, replaceStart + floor(consecutive / (numReplace + 1)) * (j + 1) - 2 + (iteration %% 5))
            }
            for (j in replacePos) {
              replaceCodon <- codonSeq[j]  # codon at position chosen for alteration
              replaceAA <- mapDict[[replaceCodon]]  # aa where codon is being replaced
              optionalCodons <- aaDict[[replaceAA]]  # synonymous codons for that AA
              replaceFreq <- toFreqDict[[replaceCodon]]
              currentChoice <- NULL
              for (k in optionalCodons) {
                if (aboveBelow[i - 1] == 0) {  # Then the last window was too low and needs to be raised
                  if (toFreqDict[[k]] > replaceFreq) {  # if optional codon would raise it
                    if (is.null(currentChoice)) {
                      currentChoice <- k
                    } else if (toFreqDict[[k]] < toFreqDict[[currentChoice]]) {
                      currentChoice <- k  # to change to the next highest synonymous codon relative to 'replaceCodon'
                    }
                  }
                } else {  # if last window was too high, needs to be lowered
                  if (toFreqDict[[k]] < replaceFreq) {
                    if (is.null(currentChoice)) {
                      currentChoice <- k
                    } else if (toFreqDict[[k]] > toFreqDict[[currentChoice]]) {
                      currentChoice <- k  # to change to the next lowest synonymous codon relative to 'replaceCodon'
                    }
                  }
                }
              }
              if (!is.null(currentChoice)) {
                codonSeq2[j] <- currentChoice
              }
              if (method == "GM") {
                harmonizedValues2 <- calculateWindowedCAI(toCAIFreqDict, codonSeq2, windowSize)
              } else if (method == "MM") {
                harmonizedValues2 <- calculateMinMax(codonSeq2, toAAFreqDict, toFreqDict, mapDict, windowSize)
              } else {
                stop("Error: Method not specified")
              }
              totalDist2 <- sum(abs(harmonizedValues2 - targetValues))
              if (totalDist2 < totalDist) {  # If there is improvement, reinitialize everything for the next iteration
                codonSeq <- codonSeq2
                harmonizedValues <- harmonizedValues2
                totalDist <- totalDist2
                flag <- 0
              }
            }
          }
          last <- aboveBelow[i]  # says sign of difference at the last codon position for the next consecutive error window
          consecutive <- 1  # resets the count
        }
      }
      if (start == totalDist) {  # then no improvement was found for this iteration
        flag <- flag + 1
      }
      iteration <- iteration + 1
    }
    return(list(codonSeq = codonSeq, harmonizedValues = harmonizedValues))
  }
