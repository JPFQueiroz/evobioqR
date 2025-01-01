#' molR
#'
#' Visualize molecular structures using a custom Mol* viewer.
#'
#' @param file The file path to a PDB file.
#' @param width Width of the widget.
#' @param height Height of the widget.
#' @param backgroundColor A string specifying the background color.
#'   Supported color names include any of the X11 colors (https://www.w3.org/TR/css-color-3/#svg-color).
#' @param selections A string specifying the chain(s) and the range of residues to select, e.g. "A 12-38, A 98-124, B 50-99".
#' @return An HTML widget displaying the molecular structure
#'
#' @references
#' Sehnal, D., Bittrich, S., Deshpande, M., Svobodová, R., Berka, K., Bazgier, V., ... & Rose, A. S. (2021).
#' Mol* Viewer: modern web app for 3D visualization and analysis of large biomolecular structures.
#' \emph{Nucleic Acids Research}, \bold{49(W1)}, W431–W437.
#'
#' @import htmlwidgets
#' @export
molR <- function(file = NULL,
                 backgroundColor = NULL,
                 selections = NULL,
                 # representationType = "ball-and-stick",
                 # colorTheme = "uniform",
                 # sizeTheme = "uniform",
                 # ignoreHydrogens = FALSE,
                 # quality = "auto",
                 # carbonColor = "chain-id",
                 # showCarbohydrateSymbol = FALSE,
                 # transparency = 0,
                 width = 600, height = 400) {
  if (is.null(file)) {
    stop("Provide `.pdb` file")
  }

  htmlwidgets::createWidget(
    name = "molR",
    x = list(file = paste(readLines(file, warn = FALSE), collapse = "\n"),
             backgroundColor = backgroundColor,
             selections = as.list(selections)#,
             # representationType = representationType,
             # colorTheme = colorTheme,
             # sizeTheme = sizeTheme,
             # ignoreHydrogens = ignoreHydrogens,
             # quality = quality,
             # carbonColor = carbonColor,
             # showCarbohydrateSymbol = showCarbohydrateSymbol,
             # transparency = transparency
             ),
    width = width,
    height = height,
    package = "evobioqR"
  )
}
