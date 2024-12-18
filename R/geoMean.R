#' @keywords internal
geoMean <- function(x) {
  exp(mean(log(x)))
}
