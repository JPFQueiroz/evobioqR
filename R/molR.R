#' molR
#'
#' Visualize molecular structures using a custom Mol* viewer embedded in R Markdown.
#'
#' @param file The file path to a PDB or MolViewSpec (.mvsj) file.
#' @param width Width of the widget.
#' @param height Height of the widget.
#' @param backgroundColor A string specifying the background color.
#'   Supported color names include any of the X11 colors (https://www.w3.org/TR/css-color-3/#svg-color).
#' @param overrideRepresentation A string vector specifying the representation type and the color theme of the polymer to add.
#'   Supported representations include: "backbone", "ball-and-stick", "cartoon", "gaussian-surface",
#'   "gaussian-volume", "interactions", "label", "line", "molecular-surface", "orientation", "point",
#'   "putty", "spacefill".
#'   Supported color themes include: "atom-id", "element-index", "element-symbol", "formal-charge",
#'   "occupancy", "uncertainty", "chain-id", "chain-instance", "entity-id", "entity-source",
#'   "model-index", "polymer-chain-id", "polymer-chain-instance", "structure-index", "cartoon",
#'   "external-volume", "illustrative", "illustrative-type", "uniform", "accessible-surface-area",
#'   "hydrophobicity", "molecule-type", "residue-name", "secondary-structure", "sequence-id".
#' @param addRepresentation A string vector specifying the additional representation type with color theme of the polymer to add.
#' @param quickStyles A string specifying the quick style to apply.
#'   Supported options are "illustrative" and "stylize current".
#' @param selectionStyles A list specifying selections with their representation type and/or color, e.g.,
#'   `list(list(selection = "A 12-38", representation = "spacefill", color = "red"), list(selection = "B 50-99", representation = "cartoon", color = "blue"))`.
#' @return An HTML widget displaying the molecular structure.
#'
#' @references
#' Sehnal, D., et al. (2021). Mol* Viewer: modern web app for 3D visualization and analysis of large biomolecular structures.
#' Nucleic Acids Research, 49(W1), W431–W437.
#' Midlik, A., et al. (2025). MolViewSpec: a Mol* extension for describing and sharing molecular visualizations.
#' Nucleic Acids Research, gkaf370.
#'
#' @import htmlwidgets
#' @import jsonlite
#' @export
molR <- function(file = NULL,
                 backgroundColor = NULL,
                 selectionStyles = NULL,
                 overrideRepresentation = NULL,
                 addRepresentation = NULL,
                 quickStyles = NULL,
                 width = 600, height = 400) {
  if (is.null(file)) {
    stop("Provide a `.pdb` or `.mvsj` file")
  }
  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  # Validate file extension
  file_ext <- tolower(tools::file_ext(file))
  if (!file_ext %in% c("pdb", "mvsj")) {
    stop("File must have a `.pdb` or `.mvsj` extension")
  }

  # Read file content based on extension
  file_content <- if (file_ext == "pdb") {
    paste(readLines(file, warn = FALSE), collapse = "\n")
  } else {
    jsonlite::read_json(file, simplifyVector = FALSE) # Keep as list for JSON compatibility
  }

  # Validate selectionStyles (only for PDB files)
  if (file_ext == "pdb") {
    valid_representations <- c("backbone", "ball-and-stick", "cartoon", "gaussian-surface", "gaussian-volume",
                               "interactions", "label", "line", "molecular-surface", "orientation", "point",
                               "putty", "spacefill")
    valid_color_themes <- c("atom-id", "element-index", "element-symbol", "formal-charge", "occupancy",
                            "uncertainty", "chain-id", "chain-instance", "entity-id", "entity-source",
                            "model-index", "polymer-chain-id", "polymer-chain-instance", "structure-index",
                            "cartoon", "external-volume", "illustrative", "illustrative-type", "uniform",
                            "accessible-surface-area", "hydrophobicity", "molecule-type", "residue-name",
                            "secondary-structure", "sequence-id")

    if (!is.null(selectionStyles)) {
      if (!is.list(selectionStyles)) {
        stop("selectionStyles must be a list of selections with representation and/or color")
      }
      for (sel in selectionStyles) {
        if (!is.character(sel$selection)) {
          stop("Each selection in selectionStyles must have a 'selection' field (e.g., 'A 12-38' or 'LIG')")
        }
        if (!is.null(sel$representation) && !sel$representation %in% valid_representations) {
          stop("Invalid representation in selectionStyles: ", sel$representation)
        }
        if (!is.null(sel$color)) {
          if (!sel$color %in% c(valid_color_themes, colors(), tolower(colors())) && !grepl("^#[0-9A-Fa-f]{6}$", sel$color)) {
            warning("Color '", sel$color, "' may not be supported. Use X11 colors, hex codes, or Mol* color themes.")
          }
        }
      }
    }
  } else {
    # Warn if parameters are provided with MVSJ files
    if (!is.null(selectionStyles) || !is.null(overrideRepresentation) || !is.null(addRepresentation) || !is.null(quickStyles)) {
      warning("Parameters 'selectionStyles', 'overrideRepresentation', 'addRepresentation', and 'quickStyles' are ignored for MVSJ files.")
    }
  }

  htmlwidgets::createWidget(
    name = "molR",
    x = list(
      file = file_content,
      fileType = file_ext, # Pass file type to JavaScript
      backgroundColor = backgroundColor,
      selectionStyles = if (file_ext == "pdb") selectionStyles else NULL,
      overrideRepresentation = if (file_ext == "pdb" && !is.null(overrideRepresentation)) list(type = overrideRepresentation[1], colorTheme = overrideRepresentation[2]) else NULL,
      addRepresentation = if (file_ext == "pdb" && !is.null(addRepresentation)) list(type = addRepresentation[1], colorTheme = addRepresentation[2]) else NULL,
      quickStyles = if (file_ext == "pdb") quickStyles else NULL
    ),
    width = width,
    height = height,
    package = "evobioqR"
  )
}
