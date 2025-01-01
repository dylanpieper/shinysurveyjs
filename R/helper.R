#' Configure Shiny Application Settings
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
#'   special_option = c(1,2,3),
#'   type_handlers = list(
#'     special_option = function(x) paste(x, collapse=",")
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

#' Clean Application Resources
#'
#' Closes the database connection pool and performs cleanup operations
#' when the application is shutting down.
#'
#' @param session Shiny session object
#'
#' @importFrom shiny onStop
#' @export
cleanup_app <- function(session) {
  shiny::onStop(function() {
    if (exists("app_pool", envir = .GlobalEnv)) {
      cleanup_pool(get("app_pool", envir = .GlobalEnv))
      rm(app_pool, envir = .GlobalEnv)
    }
  }, session)
}
