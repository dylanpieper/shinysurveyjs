#' Generate JavaScript Code for Single Survey Implementation
#'
#' @description
#' Creates a consolidated JavaScript code bundle for handling single survey functionality
#' by combining multiple JS components including configuration, cookie management,
#' progress saving, Shiny integration, initialization, and event handlers.
#'
#' @param cookie_expiration_days Numeric value specifying how many days the survey
#'   cookies should persist in the user's browser. Must be a positive integer.
#'   Default is 7 days.
#'
#' @return A character string containing the concatenated JavaScript code with all
#'   necessary components for survey functionality.
#'
#' @details
#' The function combines several JavaScript components in this order:
#' 1. Document ready wrapper
#' 2. Configuration settings (including cookie expiration)
#' 3. Cookie management utilities
#' 4. Survey progress persistence
#' 5. Shiny hidden field synchronization
#' 6. Survey initialization
#' 7. Event handlers
#'
#' All JavaScript files are read from the package's installed location and combined
#' with appropriate spacing and wrapping.
#'
#' @examples
#' # Generate JavaScript with default 7-day cookie expiration
#' js_code <- survey_single_js()
#'
#' # Generate JavaScript with 30-day cookie expiration
#' js_code <- survey_single_js(cookie_expiration_days = 30)
#'
#' @keywords internal
survey_single_js <- function(cookie_expiration_days = 7) {
  # Input validation
  if (!is.numeric(cookie_expiration_days) || cookie_expiration_days < 1) {
    stop("cookie_expiration_days must be a positive number")
  }

  # Get the package installation directory
  pkg_dir <- system.file(package = "shinysurveyjs")

  # Read all JS files
  js_files <- list(
    # Start with wrapper
    wrapper_start = "$(document).ready(function() {",

    # Core files
    config = read_asset(file.path(pkg_dir, "survey/js/config.js")),
    cookies = read_asset(file.path(pkg_dir, "survey/js/cookies.js")),
    saveSurveyProgress = read_asset(file.path(pkg_dir, "survey/js/saveSurveyProgress.js")),
    setHiddenFieldsFromShiny = read_asset(file.path(pkg_dir, "survey/js/setHiddenFieldsFromShiny.js")),
    init = read_asset(file.path(pkg_dir, "survey/js/init.js")),
    handlers = read_asset(file.path(pkg_dir, "survey/js/handlers.js")),

    # End wrapper
    wrapper_end = "});"
  )

  # Replace configuration parameters
  js_files$config <- sprintf(js_files$config, cookie_expiration_days)

  # Combine all JavaScript files in the correct order
  js_code <- paste(
    js_files$wrapper_start,
    js_files$config,
    js_files$cookies,
    js_files$saveSurveyProgress,
    js_files$setHiddenFieldsFromShiny,
    js_files$init,
    js_files$handlers,
    js_files$wrapper_end,
    sep = "\n\n"
  )

  return(js_code)
}
