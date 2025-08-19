#' Generate JavaScript Code for Single Survey Implementation
#'
#' Creates a consolidated JavaScript code bundle for handling single survey functionality,
#' including configuration, cookie management, progress saving, Shiny integration,
#' initialization, and event handlers.
#'
#' @param cookie_expiration_days Numeric. Number of days to retain survey cookies.
#'   Must be positive. Default: 7.
#'
#' @return String containing concatenated JavaScript code for survey functionality
#'
#' @examples
#' # Default 7-day cookie expiration
#' js_code <- survey_single_js()
#'
#' # Custom 30-day cookie expiration
#' js_code <- survey_single_js(cookie_expiration_days = 30)
#'
#' @noRd
#' @keywords internal
survey_single_js <- function(cookie_expiration_days = 7) {
  # Get the package installation directory
  pkg_dir <- system.file(package = "shinysurveyjs")

  # Read all JS files
  js_files <- list(
    # Start with wrapper
    wrapper_start = "$(document).ready(function() {",

    # Core files
    js_config = read_asset(file.path(pkg_dir, "js_config.js")),
    cookies = read_asset(file.path(pkg_dir, "cookies.js")),
    save_survey_progress = read_asset(file.path(pkg_dir, "save_survey_progress.js")),
    set_hidden_fields_from_shiny = read_asset(file.path(pkg_dir, "set_hidden_fields_from_shiny.js")),
    db_logic = read_asset(file.path(pkg_dir, "db_logic.js")),
    init = read_asset(file.path(pkg_dir, "init.js")),
    load_survey = read_asset(file.path(pkg_dir, "load_survey.js")),

    # End wrapper
    wrapper_end = "});"
  )

  # Replace configuration parameters
  js_files$js_config <- sprintf(js_files$js_config, cookie_expiration_days)

  # Combine JS files in the correct order
  js_code <- paste(
    js_files$wrapper_start,
    js_files$js_config,
    js_files$cookies,
    js_files$save_survey_progress,
    js_files$set_hidden_fields_from_shiny,
    js_files$db_logic,
    js_files$init,
    js_files$load_survey,
    js_files$wrapper_end,
    sep = "\n\n"
  )

  return(js_code)
}
