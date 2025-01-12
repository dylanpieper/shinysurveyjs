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
    save_survey_progress = read_asset(file.path(pkg_dir, "survey/js/save_survey_progress.js")),
    set_hidden_fields_from_shiny = read_asset(file.path(pkg_dir, "survey/js/set_hidden_fields_from_shiny.js")),
    dynamic_fields = read_asset(file.path(pkg_dir, "survey/js/dynamic_fields.js")),
    init = read_asset(file.path(pkg_dir, "survey/js/init.js")),
    load_survey = read_asset(file.path(pkg_dir, "survey/js/load_survey.js")),

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
    js_files$save_survey_progress,
    js_files$set_hidden_fields_from_shiny,
    js_files$dynamic_fields,
    js_files$init,
    js_files$load_survey,
    js_files$wrapper_end,
    sep = "\n\n"
  )

  return(js_code)
}
