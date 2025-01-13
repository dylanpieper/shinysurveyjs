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

#' Convert hex color to RGB values
#' @param hex Hex color code
#' @return Numeric vector of RGB values
#' @keywords internal
hex_to_rgb <- function(hex) {
  hex <- sub("^#", "", hex)
  if (nchar(hex) == 3) {
    hex <- paste0(rep(strsplit(hex, "")[[1]], each = 2), collapse = "")
  }
  rgb <- strsplit(hex, "(?<=..)", perl = TRUE)[[1]]
  as.numeric(paste0("0x", rgb))
}

#' Calculate relative luminance of a color
#' @param hex Hex color code
#' @return Numeric value between 0 and 1 representing relative luminance
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
      ((x + 0.055) / 1.055) ^ 2.4
    }
  })

  # Calculate relative luminance using WCAG formula
  # L = 0.2126 * R + 0.7152 * G + 0.0722 * B
  luminance <- 0.2126 * rgb_linear[1] + 0.7152 * rgb_linear[2] + 0.0722 * rgb_linear[3]

  return(luminance)
}

#' Determine if a color is light or dark
#' @param hex Hex color code
#' @param threshold Luminance threshold (default: 0.179, based on WCAG guidelines)
#' @return Boolean indicating if color is light (TRUE) or dark (FALSE)
#' @keywords internal
is_light_color <- function(hex, threshold = 0.35) {
  calculate_luminance(hex) > threshold
}

#' Get contrasting text color
#' @param background_hex Background hex color code
#' @return Hex color code for text ("#000000" for dark text or "#ffffff" for light text)
#' @keywords internal
get_contrast_color <- function(background_hex) {
  if (is_light_color(background_hex)) {
    "#000000"  # Dark text for light backgrounds
  } else {
    "#ffffff"  # Light text for dark backgrounds
  }
}

#' Generate button text color based on background
#' @param primary_hex Primary color hex code
#' @param primary_foreground Optional override for text color
#' @return Hex color code for button text
#' @keywords internal
get_button_text_color <- function(primary_hex, primary_foreground = NULL) {
  if (!is.null(primary_foreground) && !is.na(primary_foreground) && primary_foreground != "") {
    primary_foreground
  } else {
    get_contrast_color(primary_hex)
  }
}

#' Style DataTable Based on Theme Mode
#'
#' @description
#' Creates a consistent styling configuration for DataTables that matches
#' the application's theme mode (light/dark) and color scheme.
#'
#' @param mode Character string specifying "light" or "dark" mode
#' @param theme_color Hex color code for primary theme color
#' @param container_bg Background color for the container (optional)
#' @param text_color Text color (optional)
#'
#' @return List of DT options and callback functions for table styling
#'
#' @importFrom DT datatable formatStyle
#'
#' @examples
#' # Light mode table
#' DT::datatable(data, options = get_datatable_theme()$options)
#'
#' # Dark mode table
#' DT::datatable(data, options = get_datatable_theme("dark", "#003594")$options)
#'
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
  custom_css <- sprintf("
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
    dom = "frtip",  # Configure shown elements (filter, processing, table, info, pagination)
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
