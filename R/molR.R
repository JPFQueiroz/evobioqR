#' molR
#'
#' Visualize molecular structures using a custom Mol* viewer.
#'
#' @param file The file path to a PDB file.
#' @param width Width of the widget.
#' @param height Height of the widget.
#' @param backgroundColor A string specifying the background color.
#'   Supported color names include any of the X11 colors (https://www.w3.org/TR/css-color-3/#svg-color).
#' @param overrideRepresentation A string vector specifying the representation type and the color theme of the polymer to add.
#'   Supported representations include: "backbone", "ball-and-stick", "cartoon", "gaussian-surface",
#'   "gaussian-volume", "interactions", "label", "line", "molecular-surface", "orientation", "point",
#'   "putty", "spacefill".
#'
#'   Supported color themes include: "atom-id", "element-index", "element-symbol", "formal-charge",
#'   "occupancy", "uncertainty", "chain-id", "chain-instance", "entity-id", "entity-source",
#'   "model-index", "polymer-chain-id", "polymer-chain-instance", "structure-index", "cartoon",
#'   "external-volume", "illustrative", "illustrative-type", "uniform", "accessible-surface-area",
#'   "hydrophobicity", "molecule-type", "residue-name", "secondary-structure", "sequence-id".
#'
#' @param addRepresentation A string vector specifying the additional representation type with color theme of the polymer to add.
#'   Supported representations are the same as listed above.
#' @param quickStyles A string specifying the quick style to apply.
#'   Supported options are "illustrative" and "stylize current".
#' @param selections A string specifying the chain(s) and the range of residues to select, e.g. "A 12-38, A 98-124, B 50-99".
#' @return An HTML widget displaying the molecular structure.
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
                 overrideRepresentation = NULL,
                 addRepresentation = NULL,
                 quickStyles = NULL,
                 width = 600, height = 400) {
  if (is.null(file)) {
    stop("Provide `.pdb` file")
  }

  htmlwidgets::createWidget(
    name = "molR",
    x = list(file = paste(readLines(file, warn = FALSE), collapse = "\n"),
             backgroundColor = backgroundColor,
             selections = as.list(selections),
             overrideRepresentation = list(type = overrideRepresentation[1],
                                           colorTheme = overrideRepresentation[2]),
             addRepresentation = list(type = addRepresentation[1],
                                      colorTheme = addRepresentation[2]),
             quickStyles = quickStyles
             ),
    width = width,
    height = height,
    package = "evobioqR"
  )
}
