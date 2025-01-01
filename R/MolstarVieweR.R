#' MolstarVieweR
#'
#' Visualize molecular structures using the Mol* viewer
#'
#' @param pdbid A string containing a valid PDB identifier
#' @param pdbData A string containing PDB data
#' @param pdbUrl A URL to a PDB file
#' @param width Width of the widget
#' @param height Height of the widget
#' @return An HTML widget displaying the molecular structure if a valid PDB identifier, URL, or data is provided. Otherwise, only launches the Mol* viewer widget.
#'
#' @references
#' Sehnal, D., Bittrich, S., Deshpande, M., Svobodová, R., Berka, K., Bazgier, V., ... & Rose, A. S. (2021).
#' Mol* Viewer: modern web app for 3D visualization and analysis of large biomolecular structures.
#' \emph{Nucleic Acids Research}, \bold{49(W1)}, W431–W437.
#'
#' @import htmlwidgets
#' @export
MolstarVieweR <- function(pdbid = NULL, pdbData = NULL, pdbUrl = NULL,
                          width = "100%", height = "600px") {

  htmlwidgets::createWidget(
    name = "MolstarVieweR",
    x = list(pdbid = pdbid, pdbData = pdbData, pdbUrl = pdbUrl),
    width = width,
    height = height,
    package = "evobioqR"
  )
}
