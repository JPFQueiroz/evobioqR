#' @keywords internal
# This function checks each input argument to determine whether it is conforms to the necessary format for that argument. If not, the flag is set to 1 and the program exits.
checkArgs <- function(argList) {
  flag <- 0
  if (length(argList) == 7 || length(argList) == 8) {
    if (!file.exists(argList[1])) {
      cat("ERROR: No file exists at the given path for the sequence file.\n")
      flag <- 1
    }
    numMutants <- as.integer(argList[2])
    if (is.na(numMutants)) {
      cat("ERROR: The argument given for the number of desired output mutants could not be converted to an integer.\n")
      flag <- 1
    }
    windowSize <- as.integer(argList[3])
    if (is.na(windowSize) || windowSize < 1) {
      cat("ERROR: The window size must be a positive integer.\n")
      flag <- 1
    }
    if (!(argList[4] %in% c("MM", "GM"))) {
      cat("ERROR: The CUB metric argument was not recognized.\n")
      flag <- 1
    }
    if (!file.exists(argList[6])) {
      cat("ERROR: No file exists at the given path for the origin CUB table.\n")
      flag <- 1
    }
    if (length(argList) == 8 && !file.exists(argList[7])) {
      cat("ERROR: No file exists at the given path for the destination CUB table.\n")
      flag <- 1
    }
  } else {
    cat("ERROR: The code did not receive the correct number of input arguments.\n")
    flag <- 1
  }
  if (flag == 1) {
    cat("\nThe program encountered the above error(s) when attempting to read the input arguments.\n")
    cat("Please see the README file for correct input argument formats and rerun the script.\n")
    quit("no", status = 1, runLast = FALSE)  # exit R with an error status
  }
}
