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
#' @keywords internal
get_contrast_color <- function(background_hex) {
  if (is_light_color(background_hex)) {
    "#000000" # Dark text for light backgrounds
  } else {
    "#ffffff" # Light text for dark backgrounds
  }
}

#' Style DataTable Based on Theme Mode
#'
#' Creates a consistent styling configuration for DataTables that matches
#' the application's theme mode (light/dark) and color scheme.
#'
#' @param mode String. Theme mode, either "light" or "dark". Default: "light".
#' @param theme_color String. Hex color code for primary theme color.
#'   Default: "#0275d8".
#' @param container_bg String. Background color hex code for table container.
#'   Default: "#FFFFFF" for light, "#242424" for dark.
#' @param text_color String. Text color hex code.
#'   Default: "#000000" for light, "#FFFFFF" for dark.
#'
#' @return List of DataTable options and callback JavaScript for styling:
#'   * `options`: List of datatable initialization options
#'   * `callback`: JavaScript function for row hover effects
#'
#' @examples
#' \dontrun{
#' # Light mode table
#' DT::datatable(mtcars,
#'   options = get_datatable_theme()$options,
#'   callback = get_datatable_theme()$callback
#' )
#'
#' # Dark mode table with custom color
#' theme <- get_datatable_theme(
#'   mode = "dark",
#'   theme_color = "#003594"
#' )
#' DT::datatable(mtcars,
#'   options = theme$options,
#'   callback = theme$callback
#' )
#' }
#'
#' @importFrom DT datatable formatStyle
#'
#' @keywords internal
get_datatable_theme <- function(mode = "light",
                                theme_color = "#003594",
                                container_bg = NULL,
                                text_color = NULL) {
  # Define color schemes based on mode
  if (mode == "dark") {
    colors <- list(
      background = container_bg %||% "#2d2d2d",
      text = text_color %||% "#e0e0e0",
      border = "#404040",
      hover = "#404040",
      header_bg = "#1a1a1a",
      header_text = "#ffffff",
      even_row = "#333333",
      odd_row = "#2d2d2d",
      input_bg = "#404040",
      input_text = "#e0e0e0",
      input_border = "#666666"
    )
  } else {
    colors <- list(
      background = container_bg %||% "#ffffff",
      text = text_color %||% "#404040",
      border = "#e0e0e0",
      hover = "#f5f5f5",
      header_bg = "#f8f9fa",
      header_text = "#404040",
      even_row = "#ffffff",
      odd_row = "#f9f9f9",
      input_bg = "#ffffff",
      input_text = "#404040",
      input_border = "#ced4da"
    )
  }

  # Custom CSS for DataTable
  custom_css <- sprintf(
    "
    .dataTables_wrapper {
      background-color: %s;
      color: %s;
      padding: 1rem;
      border-radius: 0.5rem;
      border: 1px solid %s;
    }

    .dataTable {
      border-collapse: collapse;
      width: 100%%;
      margin-bottom: 1rem;
      background-color: %s;
    }

    .dataTable thead th {
      background-color: %s !important;
      color: %s !important;
      border-bottom: 2px solid %s !important;
      padding: 0.75rem;
      font-weight: 600;
    }

    .dataTable tbody tr {
      border-bottom: 1px solid %s;
    }

    .dataTable tbody tr:nth-of-type(even) {
      background-color: %s;
    }

    .dataTable tbody tr:nth-of-type(odd) {
      background-color: %s;
    }

    .dataTable tbody tr:hover {
      background-color: %s !important;
    }

    .dataTable tbody td {
      padding: 0.75rem;
      color: %s;
    }

    .dataTables_length select,
    .dataTables_filter input {
      background-color: %s !important;
      color: %s !important;
      border: 1px solid %s !important;
      border-radius: 0.25rem;
      padding: 0.375rem 0.75rem;
      margin: 0 0.5rem;
    }

    .dataTables_length select:focus,
    .dataTables_filter input:focus {
      border-color: %s !important;
      outline: 0;
      box-shadow: 0 0 0 0.2rem %s33;
    }

    .dataTables_info,
    .dataTables_length label,
    .dataTables_filter label,
    .dataTables_paginate {
      color: %s !important;
      margin: 1rem 0;
    }

    .paginate_button {
      background-color: %s !important;
      color: %s !important;
      border: 1px solid %s !important;
      border-radius: 0.25rem;
      padding: 0.375rem 0.75rem !important;
      margin: 0 0.25rem;
    }

    .paginate_button.current,
    .paginate_button:hover {
      background-color: %s !important;
      color: %s !important;
      border-color: %s !important;
    }

    .paginate_button.disabled {
      opacity: 0.5;
      cursor: not-allowed;
    }
  ",
    colors$background, colors$text, colors$border,
    colors$background, colors$header_bg, colors$header_text,
    colors$border, colors$border, colors$even_row,
    colors$odd_row, colors$hover, colors$text,
    colors$input_bg, colors$input_text, colors$input_border,
    theme_color, theme_color, colors$text,
    colors$background, colors$text, colors$border,
    theme_color, "#ffffff", theme_color
  )

  # DataTable options
  options <- list(
    dom = "frtip", # Configure shown elements (filter, processing, table, info, pagination)
    pageLength = 10,
    lengthMenu = list(c(5, 10, 25, 50), c("5", "10", "25", "50")),
    scrollX = TRUE,
    scrollY = "400px",
    scrollCollapse = TRUE,
    processing = TRUE,
    initComplete = DT::JS(sprintf("
      function(settings, json) {
        $(this.api().table().container()).css({
          'font-family': 'system-ui, -apple-system, sans-serif'
        });
        $(this.api().table().container()).addClass('dt-theme-%s');
        $('<style>%s</style>').appendTo('head');
      }
    ", mode, custom_css))
  )

  return(list(
    options = options,
    colors = colors
  ))
}
