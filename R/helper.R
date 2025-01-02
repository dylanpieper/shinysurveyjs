#' Configure Shiny App Settings
#'
#' @param ... Named arguments corresponding to Shiny options. Names will be prefixed with 'shiny.'
#' @param type_handlers Named list of functions to process specific options. Default handlers
#'        are provided for numeric, logical, and character values.
#'
#' @return NULL (invisibly). Sets global options for Shiny.
#'
#' @examples
#' configure_shiny(
#'   host = "0.0.0.0",
#'   port = 3838,
#'   workers = 100,
#'   sanitize_errors = TRUE,
#'   autoreload = FALSE
#' )
#'
#' # With custom type handler
#' configure_shiny(
#'   special_option = c(1, 2, 3),
#'   type_handlers = list(
#'     special_option = function(x) paste(x, collapse = ",")
#'   )
#' )
#'
#' @export
configure_shiny <- function(..., type_handlers = list()) {
  # Default type handlers
  default_handlers <- list(
    numeric = as.numeric,
    logical = as.logical,
    character = as.character
  )

  # Process all provided options
  args <- list(...)
  for (name in names(args)) {
    value <- args[[name]]

    # Apply custom handler if provided
    if (name %in% names(type_handlers)) {
      value <- type_handlers[[name]](value)
    } else {
      # Apply default handler based on value type
      handler <- default_handlers[[typeof(value)]]
      if (!is.null(handler)) {
        value <- handler(value)
      }
    }

    # Set the option with 'shiny.' prefix
    options(setNames(list(value), paste0("shiny.", name)))
  }

  invisible(NULL)
}

#' Clean Database Pool
#'
#' Closes the database connection pool and performs cleanup operations
#' when the application is shutting down.
#'
#' @param session Shiny session object
#'
#' @importFrom shiny onStop
#' @export
clean_pool <- function(session) {
  shiny::onStop(function() {
    if (exists("app_pool", envir = .GlobalEnv)) {
      cleanup_pool(get("app_pool", envir = .GlobalEnv))
      rm(app_pool, envir = .GlobalEnv)
    }
  }, session)
}

#' Hide One Message and Show Another
#'
#' @importFrom shinyjs hide show toggle
#' @param hide_id ID of the message to hide
#' @param show_id ID of the message to show
#' @param fade_time Time in seconds for fade animation (default: 1)
#' @export
hide_and_show_message <- function(hide_id, show_id, fade_time = 1) {
  # First ensure the show element exists but is hidden
  shinyjs::toggle(show_id, condition = FALSE, anim = FALSE)

  # Then do the hide animation
  shinyjs::hide(hide_id, anim = TRUE, animType = "fade", time = fade_time)

  # Use setTimeout to delay the show animation
  shinyjs::delay(fade_time * 1000, {
    shinyjs::toggle(show_id,
      condition = TRUE, anim = TRUE,
      animType = "fade", time = fade_time
    )
  })
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
