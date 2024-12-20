##' Ammonium Sulfate Saturation Calculator
#'
#' This function calculates the amount in grams of ammonium sulfate to be added to
#' a solution to achieve a desired percentage of saturation at a specified temperature.
#' It uses a linear model to interpolate ammonium sulfate properties (specific volume,
#' saturation, and molarity) between discrete temperatures (0°C to 25°C).
#'
#' \deqn{
#' \mathrm{weight (g)} = \frac{weight_{Sat} \times (M_2 - M_1)}{M_{Sat} - \left(\frac{\nu}{1000} \times 132.14 \times M_{Sat} \times M_2\right)}
#' }
#'
#' Where:
#' \itemize{
#'   \item \eqn{weight_{Sat}}: The number of grams per liter in a saturated solution of ammonium sulfate.
#'   \item \eqn{M_2}: The molarity we want to achieve.
#'   \item \eqn{M_1}: The starting molarity.
#'   \item \eqn{M_{Sat}}: The molarity of a saturated solution of ammonium sulfate.
#'   \item \eqn{\nu}: The specific volume of ammonium sulfate.
#'   \item \eqn{\mathrm{(NH_4)_2SO_4}}, 132.14 g/mol
#' }
#'
#' The function uses a linear model to interpolate values of \eqn{weight_{Sat}}, \eqn{M_{Sat}}, and \eqn{\nu}
#' based on temperature, as these properties vary with temperature. The user provides the initial
#' volume, desired saturation percentage, starting saturation percentage, and temperature.
#'
#' @param initial_volume Numeric. The initial volume of the solution in milliliters (mL).
#' @param desired_percentage Numeric. The desired percentage of ammonium sulfate saturation.
#' @param starting_percentage Numeric. The starting percentage of ammonium sulfate saturation.
#' @param temperature Numeric. The temperature in degrees Celsius (°C) at which the calculation should be made.
#'
#' @return A string. A message informing you of how many grams of ammonium sulfate
#'         should be added to achieve the desired saturation at the specified temperature.
#'
#' @references
#' Wingfield, P. (1998). Protein precipitation using ammonium sulfate.
#' \emph{Current protocols in protein science}, \bold{13(1)}, A-3F.
#'
#' @examples
#' # Example: Calculate the amount of ammonium sulfate for a 100 mL solution
#' # with a starting saturation of 30%, a desired saturation of 50%, and a temperature of 20°C.
#' saltify(100, 50, 30, 20)
#'
#' @export
#' @importFrom stats lm predict
saltify <- function(initial_volume,
                    desired_percentage,
                    starting_percentage,
                    temperature) {
  # Density and molarity of ammonium sulfate solutions
  AS <- data.frame(
    temperature = c(0, 10, 20, 25),
    specVol = c(0.5281, 0.5357, 0.5414, 0.5435),
    sat = c(515.35, 524.60, 536.49, 541.80),
    Msat = c(3.90, 3.97, 4.06, 4.10)
  )

  # Create linear models to estimate values for temperatures 0 to 25
  m1 <- lm(specVol ~ temperature, data = AS)
  m2 <- lm(sat ~ temperature, data = AS)
  m3 <- lm(Msat ~ temperature, data = AS)

  # Generate predicted values for temperatures 0 to 25
  temp_range <- 0:25
  AS_full <- data.frame(
    temperature = temp_range,
    specVol = predict(m1, newdata = data.frame(temperature = temp_range)),
    sat = predict(m2, newdata = data.frame(temperature = temp_range)),
    Msat = predict(m3, newdata = data.frame(temperature = temp_range))
  )

  # Initialize result storage
  G <- data.frame(
    temperature = AS_full$temperature,
    values = rep(NA, length(AS_full$temperature))
  )

  # Calculate the required ammonium sulfate for each temperature
  for (i in seq_along(AS_full$temperature)) {
    M1 <- starting_percentage / 100 * AS_full$Msat[i]
    M2 <- desired_percentage / 100 * AS_full$Msat[i]
    G$values[i] <- AS_full$sat[i] * (M2 - M1) /
      (AS_full$Msat[i] - (AS_full$specVol[i] / 1000 * 132.14 * AS_full$Msat[i] * M2))
  }

  # Scale by initial volume
  G$values <- G$values * initial_volume / 1000

  # Extract and format the result for the specified temperature
  result <- round(G[G$temperature == temperature, "values"], digits = 2)

  # Return formatted output message
  return(paste(
    "Add ", result, " g of ammonium sulfate to your ", initial_volume,
    " mL solution to crank up the ammonium sulfate saturation from ",
    starting_percentage, "% to ", desired_percentage,
    "% at a sizzling ", temperature,
    "\u00B0C! Get ready to rock that protein precipitation!",
    sep = ""
  ))
}
