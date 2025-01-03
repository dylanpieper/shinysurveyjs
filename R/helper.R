#' Configure Shiny App Settings
#'
#' @param ... Named arguments corresponding to Shiny options. Names will be prefixed with 'shiny.'
#' @param type_handlers Named list of functions to process specific options. Default handlers
#'        are provided for numeric, logical, and character values.
#'
#' @return NULL (invisibly). Sets global options for Shiny.
#'
#' @examples
#' \dontrun{
#' configure_shiny(
#'   host = "0.0.0.0",
#'   port = 3838,
#'   sanitize_errors = TRUE,
#'   autoreload = FALSE
#' )
#' }
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

#' Hide One Message and Show Another
#'
#' @importFrom shinyjs hide show toggle
#' @param hide_id ID of the message to hide
#' @param show_id ID of the message to show
#' @param fade_time Time in seconds for fade animation (default: 1)
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

#' Initialize Survey Environment and Database Connection
#'
#' @description
#' Initializes a complete survey environment by setting up database connections,
#' environment variables, and asynchronous processing capabilities. The function:
#' \itemize{
#'   \item Validates database configuration parameters
#'   \item Sets required environment variables if not already present
#'   \item Configures optional Shiny settings
#'   \item Establishes a global database connection pool
#'   \item Sets up asynchronous processing using future package with OS-specific configuration
#' }
#'
#' @param db_config A list containing database configuration parameters:
#'   \itemize{
#'     \item host: Database host address
#'     \item port: Database port number
#'     \item db_name: Name of the database
#'     \item user: Database username
#'     \item password: Database password
#'     \item write_table: Name of the table for write operations
#'   }
#' @param shiny_config Optional list of Shiny configuration parameters to be passed
#'   to configure_shiny function. If provided, these settings will be applied
#'   before database initialization.
#' @param workers Number of workers for parallel processing. Default is 3.
#'
#' @return Invisibly returns the initialized database pool object. The pool is also
#'   assigned to 'app_pool' in the global environment.
#'
#' @details
#' The function performs several initialization steps:
#' \itemize{
#'   \item Validates the write_table parameter is a non-empty string
#'   \item Checks for required database configuration fields
#'   \item Sets environment variables (HOST, PORT, DB_NAME, USER, PASSWORD) if not present
#'   \item Applies optional Shiny configuration
#'   \item Creates a global database connection pool if it doesn't exist
#'   \item Initializes future package for asynchronous operations based on OS
#' }
#'
#' Environment variables are only set if they don't already exist, preserving
#' any existing configurations.
#'
#' @section Database Pool:
#' The database pool is created using the db_pool_open function and stored in
#' the global environment as 'app_pool'. If a pool already exists, it is
#' not recreated.
#'
#' @section Asynchronous Processing:
#' The function detects the operating system and sets up the appropriate future plan:
#' \itemize{
#'   \item Windows: Uses multisession
#'   \item macOS: Uses multicore if supported, falls back to multisession
#'   \item Linux: Uses multicore if supported, falls back to multisession
#' }
#'
#' @importFrom cli cli_h1 cli_alert_danger cli_alert_success cli_alert_info cli_alert_warning
#' @importFrom future plan multisession multicore
#'
#' @return Invisibly returns the initialized database pool object
setup_survey <- function(db_config, shiny_config = NULL, workers = 2L) {
  # Start status group for setup process
  cli::cli_h1("Initializing Survey Environment")

  # Validate write_table parameter
  if (is.null(db_config$write_table) ||
      !is.character(db_config$write_table)
      || nchar(db_config$write_table) == 0) {
    cli::cli_alert_danger("db_config$write_table must be a non-empty character string")
    stop("Invalid write_table parameter")
  }

  # Validate database configuration
  required_db_fields <- c("host", "port", "db_name", "user", "password")
  missing_fields <- required_db_fields[!required_db_fields %in% names(db_config)]
  if (length(missing_fields) > 0) {
    cli::cli_alert_danger("Missing required database configuration fields: {.field {missing_fields}}")
    stop("Missing required database fields")
  }

  cli::cli_alert_success("Validated configuration")

  # List of environment variables to check and set
  cli::cli_h2("Environment Setup")
  env_vars <- list(
    HOST = db_config$host,
    PORT = as.character(db_config$port),
    DB_NAME = db_config$db_name,
    USER = db_config$user,
    PASSWORD = db_config$password
  )

  # Iterate through each variable and set if not exists
  cli::cli_alert_info("Checking environment variables")
  for (var_name in names(env_vars)) {
    if (Sys.getenv(var_name) == "") {
      Sys.setenv(!!var_name := env_vars[[var_name]])
      cli::cli_alert_success("Set {.env {var_name}}")
    } else {
      cli::cli_alert_success("Found existing {.env {var_name}}")
    }
  }

  # Apply Shiny configuration if provided
  if (!is.null(shiny_config)) {
    cli::cli_h2("Shiny Configuration")
    do.call(configure_shiny, shiny_config)
    cli::cli_alert_success("Shiny configuration applied")
  }

  # Initialize global database pool with error handling
  cli::cli_h2("Database Connection")
  if (!exists("app_pool", envir = .GlobalEnv)) {
    tryCatch(
      {
        assign("app_pool", do.call(
          db_pool_open,
          db_config[c("host", "port", "db_name", "user", "password")]
        ),
        envir = .GlobalEnv
        )
        cli::cli_alert_success("Started database pool")
      },
      error = function(e) {
        cli::cli_alert_danger("Failed to initialize database pool: {e$message}")
        stop("Database pool initialization failed")
      }
    )
  } else {
    cli::cli_alert_success("Using existing database pool")
  }

  # Initialize future plan based on operating system
  cli::cli_h2("Async Processing Setup")
  os <- Sys.info()["sysname"]
  cli::cli_alert_info("Configuring async processing for {.val {os}}")

  if (os == "Windows") {
    # Windows: Use multisession (fork is not available)
    future::plan(future::multisession, workers = workers)
    cli::cli_alert_success("Using multisession plan with {workers} workers")
  } else {
    # Linux/macOS: Try multicore first, fall back to multisession if not available
    tryCatch({
      future::plan(future::multicore, workers = workers)
      cli::cli_alert_success("Using multicore plan with {workers} workers")
    }, error = function(e) {
      future::plan(future::multisession, workers = workers)
      cli::cli_alert_warning("Multicore not available, falling back to multisession with {workers} workers")
    })
  }

  # Return pool object invisibly
  invisible(get("app_pool", envir = .GlobalEnv))
}
