#' Switch Visibility Between Two Elements
#'
#' Shows one element while hiding another. Simple toggle between two DIV elements
#' without animations.
#'
#' @param hide_id Character string specifying the ID of the element to hide
#' @param show_id Character string specifying the ID of the element to show
#'
#' @importFrom shinyjs hide show
#' @export
hide_and_show <- function(hide_id, show_id) {
  shinyjs::hide(hide_id)
  shinyjs::show(show_id)
}

#' Adjust Hexadecimal Color Values
#'
#' @description
#' Adjusts a hexadecimal color value by lightening or darkening it by a specified percentage.
#' The function modifies each RGB component while ensuring values stay within valid ranges (0-255).
#'
#' @param hex Character string. A hexadecimal color code (e.g., "#003594" or "003594")
#' @param percent Numeric. Percentage to adjust the color by (default: 25)
#' @param lighten Logical. If TRUE, lightens the color; if FALSE, darkens it (default: TRUE)
#'
#' @return Character string. The adjusted hexadecimal color code with leading "#"
#'
#' @examples
#' # Lighten a color by 25%
#' adjust_hex("#003594") # Default lighten by 25%
#'
#' # Darken a color by 30%
#' adjust_hex("#003594", percent = 30, lighten = FALSE)
#'
#' # Lighten a color by 50%
#' adjust_hex("003594", percent = 50) # Works with or without leading "#"
#'
#' @keywords internal
adjust_hex <- function(hex, percent = 25, lighten = TRUE) {
  hex <- gsub("^#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16)
  g <- strtoi(substr(hex, 3, 4), 16)
  b <- strtoi(substr(hex, 5, 6), 16)

  if (lighten) {
    r <- min(255, r + (255 - r) * percent / 100)
    g <- min(255, g + (255 - g) * percent / 100)
    b <- min(255, b + (255 - b) * percent / 100)
  } else {
    r <- max(0, r * (1 - percent / 100))
    g <- max(0, g * (1 - percent / 100))
    b <- max(0, b * (1 - percent / 100))
  }

  sprintf("#%02x%02x%02x", round(r), round(g), round(b))
}
