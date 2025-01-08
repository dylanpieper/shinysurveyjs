#' Read Asset Files
#'
#' Reads JavaScript or CSS files.
#'
#' @param filepath Character string. Path to the file to be read.
#'
#' @return A character string containing the file contents.
#'
#' @details
#' This function handles reading JavaScript and CSS files, validating the file
#' exists and has the correct extension before reading.
#'
#' @examples
#' \dontrun{
#' # Read a JavaScript file
#' js_content <- read_asset("path/to/script.js")
#'
#' # Read a CSS file
#' css_content <- read_asset("path/to/styles.css")
#' }
#'
#' @keywords internal
read_asset <- function(filepath) {
  # Check file existence
  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }

  # Get file extension and validate
  type <- tools::file_ext(filepath)
  if (!type %in% c("js", "css")) {
    stop("Only .js and .css files are supported")
  }

  # Read content
  content <- readLines(filepath, warn = FALSE) |> paste(collapse = "\n")

  return(content)
}

#' Switch Visibility Between Two Elements
#'
#' Shows one element while hiding another. Simple toggle between two DIV elements
#' without animations.
#'
#' @param hide_id Character string specifying the ID of the element to hide
#' @param show_id Character string specifying the ID of the element to show
#'
#' @importFrom shinyjs hide show
#' @keywords internal
hide_and_show <- function(hide_id, show_id) {
  shinyjs::hide(hide_id)
  shinyjs::show(show_id)
}

#' Adjust Hex Color Brightness
#'
#' Adjusts a hex color's brightness by a specified percentage. Positive percentages
#' lighten the color (moving towards white), while negative percentages darken it
#' (moving towards black).
#'
#' @param hex A character string representing a hex color code (e.g., "#FF0000" or "FF0000")
#' @param percent Numeric value between -100 and 100 for the adjustment percentage:
#'               positive values lighten, negative values darken (default: 25)
#'
#' @return A character string containing the adjusted hex color code with leading "#"
#'
#' @examples
#' # Lighten a color by 25%
#' adjust_hex("#FF0000", 25)    # Makes red lighter
#'
#' # Darken a color by 30%
#' adjust_hex("#00FF00", -30)   # Makes green darker
#'
#' # Lighten without leading "#"
#' adjust_hex("0000FF", 20)     # Makes blue lighter
#'
#' @keywords internal
adjust_hex <- function(hex, percent = 25) {
  hex <- gsub("^#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16)
  g <- strtoi(substr(hex, 3, 4), 16)
  b <- strtoi(substr(hex, 5, 6), 16)

  if (percent >= 0) {
    r <- min(255, r + (255 - r) * percent / 100)
    g <- min(255, g + (255 - g) * percent / 100)
    b <- min(255, b + (255 - b) * percent / 100)
  } else {
    # Convert negative percent to positive for calculation
    adj_percent <- abs(percent)
    r <- max(0, r * (1 - adj_percent / 100))
    g <- max(0, g * (1 - adj_percent / 100))
    b <- max(0, b * (1 - adj_percent / 100))
  }

  sprintf("#%02x%02x%02x", round(r), round(g), round(b))
}
