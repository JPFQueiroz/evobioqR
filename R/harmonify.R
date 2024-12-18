utils::globalVariables(c("codonPositon", "Sequence", "Value"))
##' Performs codon harmonization with the CHARMING algorithm
#'
#' This function is a re-implementation in R of the Python script CHARMIMG.py (https://github.com/wrightgs/CHARMING) that implements the codon harmonization algorithm CHARMING (Codon HARMonizING) described in Wright et al. (2022).
#'
#' @param seq_path String. The file path to a sequence you would like to harmonize in FASTA format.
#' @param numMutants Numeric. The desired number of synonymous harmonized mutants. Must be a integer greater than 1 (default = 3).
#' @param windowSize Numeric. The size of the sliding window to use with the desired codon usage measure. This value must be a positive integer greater than 1 (default = 10).
#' @param measure String. The desired codon calculator, must be either "MM" (for percent MinMax) or "GM" (for geometric mean). Defaults to "MM".
#' @param outName String. The name (without spaces) you wishes to associate with the harmonization.
#' @param codonTableOrigin String. The path to the codon usage table file for the origin species.
#' @param codonTableDestination String. The path to the codon usage table file for the destination species.
#' @param save_plots Logical. Generate plots.
#' @param save_fasta Logical. Generate FASTA files containing the harmonized mutants.
#'
#' @return A list containing:
#'
#' \code{[[1]]}
#' A matrix with the following structure:
#'
#' \describe{
#'   \item{\code{[,1]}}{Numeric. Represents the calculated distances or metrics.}
#'   \item{\code{[,2]}}{Character. Contains the codon sequences corresponding to the rows.}
#'   \item{\code{[,3]}}{Numeric vector. Represents a list of metrics or values associated with each codon.}
#' }
#' Each row corresponds to a harmonized sequence, containing:
#' \enumerate{
#'   \item The distance (Column 1).
#'   \item The sequence (Column 2).
#'   \item Associated metrics (Column 3).
#' }
#'
#' \code{[[2]]}
#' A tibble data frame in wide format.
#'
#' This tibble includes:
#' \itemize{
#'   \item The codon positions of the input sequence.
#'   \item The codon usage metrics for the sequence in the source organism.
#'   \item The codon usage metrics of the same sequence in the destination organism.
#'   \item The codon usage metrics for the harmonized sequences requested for the destination organism.
#' }
#'
#' @references
#' Wright, G., Rodriguez, A., Li, J., Milenkovic, T., Emrich, S. J., & Clark,
#' P. L. (2022). CHARMING: Harmonizing synonymous codon usage to replicate a
#' desired codon usage pattern. \emph{Protein Science}, \bold{31(1)}, 221-231.
#'
#' @examples
#' # Example: Perform codon harmonization using CHARMING
#' seq_path <- system.file("extdata", "MAD3_Scer.txt", package = "evobioqR")
#' codonTableOrigin <- system.file("extdata", "ScerCUB.txt", package = "evobioqR")
#' codonTableDestination <- system.file("extdata", "EcolCUB.txt", package = "evobioqR")
#' harmonify(seq_path = seq_path,
#'           numMutants = 2,
#'           windowSize = 17,
#'           measure = "MM",
#'           outName = "test",
#'           codonTableOrigin = codonTableOrigin,
#'           codonTableDestination = codonTableDestination,
#'           save_plots = FALSE,
#'           save_fasta = FALSE)
#'
#' @export
#' @importFrom utils write.table
#' @importFrom stats cor
#' @importFrom dplyr %>% filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line labs theme_classic scale_color_manual ggsave
#' @importFrom tibble tibble
harmonify <- function(seq_path,
                      numMutants = 3,
                      windowSize = 10,
                      measure = "MM",
                      outName,
                      codonTableOrigin,
                      codonTableDestination,
                      save_plots = TRUE,
                      save_fasta = TRUE) {
  if (is.null(codonTableDestination)) {
    args <- c("RStudio",
              seq_path,
              numMutants,
              windowSize,
              measure,
              outName,
              codonTableOrigin)
  } else {
    args <- c("RStudio",
              seq_path,
              numMutants,
              windowSize,
              measure,
              outName,
              codonTableOrigin,
              codonTableDestination)
  }
  cat(args, "\n")
  # Check if harmonizing sequence to native species
  if (length(args) == 7) {
    nucSeq <- readInputSeq(args[2])
    numHarmonized <- as.integer(args[3])
    windowSize <- as.integer(args[4])
    model <- args[5]
    outName <- args[6]
    freqDict <- readCUBTable(args[7])
    aaFreqDict <- generateAAFreqDict(freqDict, aaDict)
    codonSeq <- generateCodonSeq(nucSeq$stringSeq)
    randomSeeds <- generateRandomSeqs(codonSeq, numHarmonized * 10)
    outputs <- list()
    counter <- 0
    if (model == "MM") {
      targetVals <- calculateMinMax(codonSeq, aaFreqDict, freqDict, mapDict,
                                    windowSize)
      for (sequence in randomSeeds) {
        initVals <- calculateMinMax(sequence, aaFreqDict, freqDict, mapDict,
                                    windowSize)
        result <- harmonizedSeqErrorCorrection(sequence, initVals, targetVals,
                                               freqDict, aaFreqDict, NULL,
                                               windowSize, model)
        finalSeq <- result[[1]]
        finalVals <- result[[2]]
        finalDist <- sum(abs(finalVals - targetVals))
        outputs[[counter + 1]] <- list(finalDist, finalSeq, finalVals)
        counter <- counter + 1
        if (counter %% 10 == 0) {
          cat("Harmonized mutant #", counter %/% 10, "complete\n")
        }
      }
    } else if (model == "GM") {
      targetVals <- calculateWindowedCAI(freqDict, codonSeq, windowSize)
      for (sequence in randomSeeds) {
        initVals <- calculateWindowedCAI(freqDict, sequence, windowSize)
        result <- harmonizedSeqErrorCorrection(sequence, initVals, targetVals,
                                               freqDict, NULL, freqDict,
                                               windowSize, model)
        finalSeq <- result[[1]]
        finalVals <- result[[2]]
        finalDist <- sum(abs(finalVals - targetVals))
        outputs[[counter + 1]] <- list(finalDist, finalSeq, finalVals)
        counter <- counter + 1
        if (counter %% 10 == 0) {
          cat("Harmonized mutant #", counter %/% 10, "complete\n")
        }
      }
    }
    # When harmonizing from one species for production in another
  } else if (length(args) == 8) {
    nucSeq <- readInputSeq(args[2])
    numHarmonized <- as.integer(args[3])
    windowSize <- as.integer(args[4])
    model <- args[5]
    outName <- args[6]
    freqDictNative <- readCUBTable(args[7])
    freqDictHost <- readCUBTable(args[8])
    aaFreqDictNative <- generateAAFreqDict(freqDictNative, aaDict)
    aaFreqDictHost <- generateAAFreqDict(freqDictHost, aaDict)
    codonSeq <- generateCodonSeq(nucSeq$stringSeq)
    randomSeeds <- generateRandomSeqs(codonSeq, numHarmonized * 10)
    # Swap first two sequences in randomSeeds with the WT sequence
    randomSeeds[[1]] <- codonSeq
    if (length(randomSeeds) > 1) {
      randomSeeds[[2]] <- codonSeq
    }
    outputs <- list()
    counter <- 0
    if (model == "MM") {
      targetVals <- calculateMinMax(codonSeq, aaFreqDictNative, freqDictNative,
                                    mapDict, windowSize)
      unharmonizedVals <- calculateMinMax(codonSeq, aaFreqDictHost, freqDictHost,
                                          mapDict, windowSize)
      for (sequence in randomSeeds) {
        if (counter == 0) { # Use rodriguez harmonization to initialize first sequence
          initResult <- initializeHarmonizedSequence(sequence, freqDictNative,
                                                     freqDictHost, aaFreqDictHost,
                                                     NULL, windowSize, model)
          initSeq <- initResult[[1]]
          initVals <- initResult[[2]]
          finalResult <- harmonizedSeqErrorCorrection(initSeq, initVals,
                                                      targetVals, freqDictHost,
                                                      aaFreqDictHost, NULL,
                                                      windowSize, model)
        } else {
          initVals <- calculateMinMax(sequence, aaFreqDictHost, freqDictHost,
                                      mapDict, windowSize)
          finalResult <- harmonizedSeqErrorCorrection(sequence, initVals,
                                                      targetVals, freqDictHost,
                                                      aaFreqDictHost, NULL,
                                                      windowSize, model)
        }
        finalSeq <- finalResult[[1]]
        finalVals <- finalResult[[2]]
        finalDist <- sum(abs(finalVals - targetVals))
        outputs[[counter + 1]] <- list(finalDist, finalSeq, finalVals)
        counter <- counter + 1
        if (counter %% 10 == 0) {
          cat("Harmonized mutant #", counter %/% 10, "complete\n")
        }
      }
    } else if (model == "GM") {
      targetVals <- calculateWindowedCAI(freqDictNative, codonSeq, windowSize)
      unharmonizedVals <- calculateWindowedCAI(freqDictHost, codonSeq, windowSize)
      for (sequence in randomSeeds) {
        if (counter == 0) { # Use Rodriguez harmonization to initialize first sequence
          initResult <- initializeHarmonizedSequence(sequence, freqDictNative,
                                                     freqDictHost, aaFreqDictHost,
                                                     freqDictHost, windowSize,
                                                     model)
          initSeq <- initResult[[1]]
          initVals <- initResult[[2]]
          finalResult <- harmonizedSeqErrorCorrection(initSeq, initVals,
                                                      targetVals, freqDictHost,
                                                      aaFreqDictHost,
                                                      freqDictHost, windowSize,
                                                      model)
        } else {
          initVals <- calculateWindowedCAI(freqDictHost, sequence, windowSize)
          finalResult <- harmonizedSeqErrorCorrection(sequence, initVals,
                                                      targetVals, freqDictHost,
                                                      aaFreqDictHost,
                                                      freqDictHost, windowSize,
                                                      model)
        }
        finalSeq <- finalResult[[1]]
        finalVals <- finalResult[[2]]
        finalDist <- sum(abs(finalVals - targetVals))
        outputs[[counter + 1]] <- list(finalDist, finalSeq, finalVals)
        counter <- counter + 1
        if (counter %% 10 == 0) {
          cat("Harmonized mutant #", counter %/% 10, "complete\n")
        }
      }
    }
  }
  outputs <- do.call(rbind, outputs)
  outputs <- outputs[order(as.numeric(outputs[,1])), ]
  # outFile <- file(paste(outName, model, "harmonized_sequences.txt",
  #                       sep = "_"), "w")
  # outFile2 <- file(paste(outName, model, "harmonized_values.txt",
  #                        sep = "_"), "w")
  # if (length(args) == 8) {
  #   writeLines(paste("Unharmonized CUB metric values: ",
  #                    toString(unharmonizedVals)), outFile2)
  # }
  # writeLines(paste("Target CUB metric values: ", toString(targetVals)), outFile2)
  # write.table(outputs, file = outFile, col.names = FALSE, row.names = FALSE,
  #             quote = FALSE, sep = "\t")
  if (save_fasta) {
    for (seqNum in 1:numMutants) {
      seqCorr <- cor(outputs[seqNum, 3][[1]], targetVals)
      outStr <- paste(outputs[seqNum, 2][[1]], collapse = "")
      gc <- (sum(unlist(gregexpr("[GC]", outStr)) > 0) / nchar(outStr)) * 100
      outFile3 <- file(paste0(outName, "_", model, "_harmonized_sequences.fasta"), "a")
      # outFile2 <- file(paste0(outName, "_", model, "_harmonized_values.txt"), "a")
      cat(paste(">Harmonized Sequence ", seqNum, ", %GC = ", round(gc, 1),
                " Net distance from target values = ",
                round(outputs[seqNum, 1][[1]], 1),
                ", Pearson correlation ", round(seqCorr, 4),
                " with target sequence.\n", sep = ""),
          file = outFile3)
      cat(outStr, "\n", file = outFile3)
      # cat("Harmonized Sequence ", seqNum, " CUB metric values: ",
      #     outputs[seqNum, 3][[1]], "\n", file = outFile2)
    }
    # close(outFile)
    # close(outFile2)
    close(outFile3)
  }
  # Create a tibble and plot the data
  result <- tibble(
    codonPositon = 1:length(targetVals),
    WT = targetVals,
    unharmonized = unharmonizedVals
  )
  # Add harmonized sequences dynamically
  for (i in 1:nrow(outputs)) {
    result[[paste0("harmonizedSeq", i)]] <- unlist(outputs[i, 3])
  }
  # Pivot the data to long format
  result_long <- result %>%
    pivot_longer(cols = -codonPositon, names_to = "Sequence", values_to = "Value")
  # Generate and save plots for each harmonized sequence
  for (i in 1:numMutants) {
    harmonized_col <- paste0("harmonizedSeq", i)
    filtered_data <- result_long %>%
      filter(Sequence %in% c("WT", "unharmonized", harmonized_col))
    # Ensure correct order of factor levels for Sequence
    filtered_data$Sequence <- factor(
      filtered_data$Sequence,
      levels = c("WT", "unharmonized", harmonized_col), # Ensure the order matches your labels
      labels = c("Wild-type", "Unharmonized", "Harmonized")
    )
    plot <- ggplot(filtered_data, aes(x = codonPositon, y = Value, color = Sequence)) +
      geom_line(linewidth = 1) +
      labs(
        title = paste("Comparison of codon usage in the wild-type, unharmonized, and harmonized sequence ", i),
        x = "Codon position",
        y = "% MinMax"
      ) +
      theme_classic() +
      scale_color_manual(
        values = c("Wild-type" = "#7FC97F",
                   "Unharmonized" = "#BEAED4",
                   "Harmonized" = "#FDC086")
      )
    # Save the plot as a PNG file
    if (save_plots) {
      ggsave(
        filename = paste0("harmonized_sequence_", i, ".png"),
        plot = plot,
        width = 8, height = 6, dpi = 300
      )
    }
  }
  colnames(outputs) <- c("Distance", "Sequence", "Metrics")
  return(list(outputs, result))
}
