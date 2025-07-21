#' Read JavaScript or CSS Asset Files
#'
#' Reads and validates JavaScript (.js) or CSS (.css) asset files from disk.
#'
#' @param filepath String. Path to the JavaScript or CSS file.
#'
#' @return String containing the file contents.
#'
#' @examples
#' \dontrun{
#' js_content <- read_asset("www/custom.js")
#' css_content <- read_asset("www/theme.css")
#' }
#'
#' @noRd
#'
#' @noRd
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
#' Shows one element while hiding another element via shinyjs. Provides a simple
#' toggle between two HTML elements without animations.
#'
#' @param hide_id String. ID of the element to hide.
#' @param show_id String. ID of the element to show.
#'
#' @importFrom shinyjs hide show
#'
#' @noRd
#' @keywords internal
hide_and_show <- function(hide_id, show_id) {
  shinyjs::hide(hide_id)
  shinyjs::show(show_id)
}

#' Adjust Hex Color Brightness
#'
#' Adjusts the brightness of a hex color code by a specified percentage. Positive
#' percentages lighten the color (towards white), negative percentages darken it
#' (towards black).
#'
#' @param hex String. Hex color code, with or without leading "#" (e.g., "#FF0000"
#'   or "FF0000").
#' @param percent Numeric. Adjustment percentage between -100 and 100. Positive values
#'   lighten, negative values darken. Default: 25.
#'
#' @return String. Adjusted hex color code with leading "#".
#'
#' @examples
#' # Lighten red by 25%
#' adjust_hex("#FF0000", 25)
#'
#' # Darken green by 30%
#' adjust_hex("#00FF00", -30)
#'
#' # Lighten blue by 20% (without leading "#")
#' adjust_hex("0000FF", 20)
#'
#' @noRd
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

#' Convert Hexadecimal Color Code to RGB Values
#'
#' Converts a hex color code (e.g., "#FF0000") to a numeric vector of RGB values
#' between 0 and 255.
#'
#' @param hex Character. Hex color code, with or without leading "#".
#'
#' @return Numeric vector of length 3 containing RGB values (0-255).
#'
#' @examples
#' hex_to_rgb("#FF0000")  # Returns c(255, 0, 0)
#' hex_to_rgb("00FF00")   # Returns c(0, 255, 0)
#'
#' @noRd
#' @keywords internal
hex_to_rgb <- function(hex) {
  hex <- sub("^#", "", hex)
  if (nchar(hex) == 3) {
    hex <- paste0(rep(strsplit(hex, "")[[1]], each = 2), collapse = "")
  }
  rgb <- strsplit(hex, "(?<=..)", perl = TRUE)[[1]]
  as.numeric(paste0("0x", rgb))
}

#' Calculate Relative Luminance From Color
#'
#' Calculates the relative luminance of a color according to WCAG 2.0 definition.
#' Relative luminance represents the relative brightness of a color where 0 is
#' black and 1 is white.
#'
#' @param hex String. Hex color code in "#RRGGBB" format.
#' @return Numeric between 0 and 1 representing the relative luminance.
#'
#' @references
#' WCAG 2.0: <https://www.w3.org/TR/WCAG20/#relativeluminancedef>
#'
#' @noRd
#' @keywords internal
calculate_luminance <- function(hex) {
  # Convert hex to RGB
  rgb <- hex_to_rgb(hex)

  # Normalize RGB values to 0-1
  rgb_norm <- rgb / 255

  # Convert to linear RGB values
  rgb_linear <- sapply(rgb_norm, function(x) {
    if (x <= 0.03928) {
      x / 12.92
    } else {
      ((x + 0.055) / 1.055)^2.4
    }
  })

  # Calculate relative luminance using WCAG formula
  # L = 0.2126 * R + 0.7152 * G + 0.0722 * B
  luminance <- 0.2126 * rgb_linear[1] + 0.7152 * rgb_linear[2] + 0.0722 * rgb_linear[3]

  return(luminance)
}

#' Determine if a Color is Light or Dark
#'
#' Calculates the relative luminance of a hex color code and compares it against
#' a threshold to determine if the color should be considered light or dark.
#'
#' @param hex Character. Hex color code (e.g., "#FFFFFF").
#' @param threshold Numeric. Luminance threshold between 0 and 1.
#'   Values above threshold are considered light.
#'   Default: 0.35
#'
#' @return Logical. `TRUE` if color is light, `FALSE` if dark.
#'
#' @noRd
#' @keywords internal
is_light_color <- function(hex, threshold = 0.35) {
  calculate_luminance(hex) > threshold
}

#' Get Contrasting Text Color
#'
#' Determines the optimal text color (black or white) based on background color
#' brightness using W3C compliant relative luminance calculation.
#'
#' @param background_hex String. Background hex color code (e.g., "#123456").
#'
#' @return String. Hex color code for text: "#000000" for dark text or "#FFFFFF" for
#'   light text to ensure WCAG contrast requirements are met.
#'
#' @noRd
#' @keywords internal
get_contrast_color <- function(background_hex) {
  if (is_light_color(background_hex)) {
    "#000000" # Dark text for light backgrounds
  } else {
    "#ffffff" # Light text for dark backgrounds
  }
}
