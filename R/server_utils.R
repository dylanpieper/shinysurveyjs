#' Setup Global Survey Environment and Database Connection
#'
#' @description
#' Setup the global survey environment by creating database connections,
#' environment variables, and a future asynchronous processing plan. The function:
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
#'     \item **host**: Database host address
#'     \item **port**: Database port number
#'     \item **db_name**: Name of the database
#'     \item **user**: Database username
#'     \item **password**: Database password
#'     \item **write_table**: Name of the table for write operations
#'   }
#' @param shiny_config Optional list of Shiny configuration parameters to be passed
#'   to configure_shiny function. If provided, these settings will be applied
#'   before database initialization.
#' @param workers Number of workers for parallel processing. Default is 3.
#'
#' @return Invisibly returns the database pool object. The pool is also assigned to
#'   'app_pool' in the global environment.
#'
#' @details
#' The function performs several initialization steps:
#' \itemize{
#'   \item Validates the write_table parameter is a non-empty string
#'   \item Checks for required database configuration fields
#'   \item Sets environment variables (**HOST**, **PORT**, **DB_NAME**, **USER**, **PASSWORD**) if not present
#'   \item Applies optional Shiny configuration
#'   \item Creates a global database connection pool if it doesn't exist
#'   \item Initializes future package for asynchronous operations based on OS
#' }
#'
#' Environment variables are only set if they don't already exist, preserving
#' any existing configurations.
#'
#' @section Database Pool:
#' The database pool is created using the `db_pool_open` function and stored in
#' the global environment as 'app_pool'. If a pool already exists, it is
#' not recreated.
#'
#' @section Asynchronous Processing:
#' The function detects the operating system and sets up the appropriate future plan:
#' \itemize{
#'   \item **Windows**: Uses `multisession`
#'   \item **macOS**: Uses `multicore` if supported, falls back to `multisession`
#'   \item **Linux**: Uses `multicore` if supported, falls back to `multisession`
#' }
#'
#' @importFrom cli cli_h1 cli_alert_danger cli_alert_success cli_alert_info cli_alert_warning
#' @importFrom future plan multisession multicore
#'
#' @return Invisibly returns the initialized database pool object
#'
#' @keywords internal
survey_setup <- function(db_config, shiny_config = NULL, workers = 2L) {
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
      do.call(Sys.setenv, setNames(list(env_vars[[var_name]]), var_name))
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
#'
#' @keywords internal
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

#' Setup Server Components for Survey Application
#'
#' @description
#' Setup the server-side components of the survey application by setting up
#' logging and database operations in the parent environment. Creates a new logger instance
#' and attempts to establish database operations, with error handling for database
#' initialization failures.
#'
#' @param session A Shiny session object containing the session token
#' @param db_config A list containing database configuration with elements:
#'   \itemize{
#'     \item log_table: Name of the logging table in the database
#'     \item write_table: Name of the survey table in the database
#'   }
#' @param app_pool A database connection pool object from the global environment
#' @param survey_logger A reference class object for logging functionality
#' @param db_ops A reference class object for database operations
#'
#' @details
#' This function creates two objects in the parent environment:
#'   \itemize{
#'     \item logger: The initialized survey logger object
#'     \item db_ops: The initialized database operations object (or NULL if initialization failed)
#'   }
#'
#' @examples
#' \dontrun{
#' server_setup(
#'   session = session,
#'   db_config = db_config,
#'   app_pool = app_pool,
#'   survey_logger = survey_logger,
#'   db_ops = db_ops
#' )
#' # After running, 'logger' and 'db_ops' are available in the parent environment
#' }
#'
#' @keywords internal
server_setup <- function(session, db_config, app_pool, survey_logger, db_ops, suppress_logs) {
  # Initialize survey app logger
  logger <- survey_logger$new(
    log_table = db_config$log_table,
    session_id = session$token,
    survey_name = db_config$write_table,
    suppress_logs = suppress_logs
  )

  logger$log_message("Started session", zone = "SURVEY")

  # Initialize database operations with error logging
  db_operations <- tryCatch(
    {
      db_ops$new(app_pool, session$token, logger)
    },
    error = function(e) {
      msg <- sprintf("Failed to initialize db_ops: %s", e$message)
      logger$log_message(msg, type = "ERROR", zone = "DATABASE")
      NULL
    }
  )

  # Assign objects to parent environment
  assign("logger", logger, envir = parent.frame())
  assign("db_ops", db_operations, envir = parent.frame())

  invisible()
}

#' Create Survey Response Table Output
#'
#' This function sets up the server-side logic for displaying a survey response
#' data table in a Shiny application. It handles the rendering of the response
#' table, controls its visibility, and applies theming based on light/dark mode.
#'
#' @param output The Shiny output object
#' @param rv A reactive values object containing:
#'   \itemize{
#'     \item survey_completed - Boolean indicating if survey is completed
#'     \item loading - Boolean indicating loading state
#'     \item survey_responses - Data frame of survey responses
#'     \item error_message - String containing error message if any
#'   }
#' @param show_response Boolean indicating whether to show the response table
#' @param theme_mode Character string specifying the theme mode ("light" or "dark")
#' @param theme_color Character string specifying the primary theme color (hex code)
#'
#' @return None (called for side effects)
#'
#' @details
#' The function creates two reactive outputs:
#' \itemize{
#'   \item surveyResponseTable - A DataTable showing survey responses with themed styling
#'   \item showResponseTable - Controls visibility of the response table
#' }
#'
#' The table will only be shown when:
#' \itemize{
#'   \item The survey is completed
#'   \item Data is not loading
#'   \item There are no error messages
#'   \item show_response parameter is TRUE
#' }
#'
#' The function applies different color schemes based on the theme_mode:
#' \itemize{
#'   \item Light mode: White background with dark text and subtle borders
#'   \item Dark mode: Dark background with light text and contrasting borders
#' }
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   rv <- reactiveValues(
#'     survey_completed = FALSE,
#'     loading = FALSE,
#'     survey_responses = data.frame(),
#'     error_message = NULL
#'   )
#'
#'   server_response(
#'     output,
#'     rv,
#'     show_response = TRUE,
#'     theme_mode = "light",
#'     theme_color = "#003594"
#'   )
#' }
#' }
#'
#' @importFrom shiny req validate need reactive outputOptions
#' @importFrom DT renderDT datatable formatStyle
#'
#' @keywords internal
server_response <- function(output, rv, show_response = TRUE, theme_mode = "light", theme_color = "#003594") {
  # Get theme colors based on mode
  colors <- if (theme_mode == "dark") {
    list(
      background = "#2d2d2d",
      text = "#e0e0e0",
      border = "#404040",
      header_bg = "#1a1a1a"
    )
  } else {
    list(
      background = "#ffffff",
      text = "#404040",
      border = "#e0e0e0",
      header_bg = "#f5f5f5"
    )
  }

  # Render the response table
  output$surveyResponseTable <- DT::renderDT({
    req(rv$survey_completed)
    req(rv$survey_responses)

    if (!is.null(rv$error_message)) {
      return(NULL)
    }

    # Create a simple DataTable with just horizontal scrolling
    DT::datatable(
      rv$survey_responses,
      options = list(
        scrollX = TRUE,
        dom = "t",  # Show only the table, no controls
        ordering = FALSE,  # Disable sorting
        initComplete = DT::JS(sprintf(
          "function(settings, json) {
            $(this.api().table().container()).css({
              'background-color': '%s',
              'color': '%s'
            });
            $('table.dataTable thead th').css({
              'background-color': '%s',
              'border-bottom': '1px solid %s',
              'font-weight': 'bold'
            });
          }",
          colors$background,
          colors$text,
          colors$header_bg,
          colors$border
        ))
      ),
      rownames = FALSE,
      selection = "none",
      class = "display"
    ) |>
      DT::formatStyle(
        columns = names(rv$survey_responses),
        backgroundColor = colors$background,
        color = colors$text
      )
  })

  # Control response table visibility
  output$showResponseTable <- reactive({
    show_response && rv$survey_completed && is.null(rv$error_message)
  })

  # Set output options
  outputOptions(output, "showResponseTable", suspendWhenHidden = FALSE)
}

#' Clean Up Server Session Resources
#'
#' This function handles cleanup tasks when a Shiny session ends. It ensures proper
#' resource disposal by logging the session end and closing database connections.
#'
#' @param session The Shiny session object
#' @param logger A logger object with a log_message method for recording events
#' @param zone Character string specifying the logging zone (default: "SURVEY")
#'
#' @return None (called for side effects)
#'
#' @details
#' The function performs the following cleanup tasks:
#' \itemize{
#'   \item Logs the session end event
#'   \item Closes any open database pool connections
#' }
#'
#' This function should be called within the server function of a Shiny application
#' to ensure proper resource management.
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   # Setup logger
#'   logger <- LoggerFactory$new()
#'
#'   # Register cleanup
#'   server_clean(session, logger)
#' }
#' }
#'
#' @importFrom shiny onSessionEnded
#'
#' @keywords internal
server_clean <- function(session, logger, zone = "SURVEY") {
  # Register cleanup actions for session end
  session$onSessionEnded(function() {
    # Log session end
    logger$log_message("Ended session", zone = zone)

    # Close database connections
    db_pool_close(session)
  })
}

#' Parse URL Query Parameters
#'
#' Extracts query parameters from either a URL string or a Shiny session object
#' and returns them as a named list. The function handles URL encoding, empty values,
#' and multiple parameters with the same name.
#'
#' @param input Either a character string containing a URL with query parameters,
#'              or a Shiny session object
#' @return A named list where names are parameter names and values are parameter values
#' @examples
#' # From URL string
#' parse_query("https://example.com/page?name=John&age=25")
#'
#' # From Shiny session (within Shiny server function)
#' server <- function(input, output, session) {
#'   params <- parse_query(session)
#' }
#'
#' @keywords internal
parse_query <- function(input) {
  # Check if input is a Shiny session object
  if (inherits(input, "ShinySession")) {
    # Extract query string from Shiny session
    query_string <- input$clientData$url_search
    # Remove leading '?' if present
    query_string <- sub("^\\?", "", query_string)
  } else if (is.character(input)) {
    # Extract query string after '?'
    query_string <- sub(".*\\?", "", input)
  } else {
    stop("Input must be either a URL string or a Shiny session object")
  }

  # Return empty list if no query parameters
  if (is.null(query_string) || query_string == "") {
    return(list())
  }

  # Split parameters into key-value pairs
  params <- strsplit(query_string, "&")[[1]]

  # Initialize empty list for results
  result <- list()

  # Process each parameter
  for (param in params) {
    # Skip empty parameters
    if (param == "") next

    # Split into key and value
    parts <- strsplit(param, "=")[[1]]
    key <- parts[1]

    # Handle cases where value might be missing
    value <- if (length(parts) > 1) {
      utils::URLdecode(parts[2])
    } else {
      NA
    }

    # If key already exists, convert to vector
    if (key %in% names(result)) {
      if (is.vector(result[[key]])) {
        result[[key]] <- c(result[[key]], value)
      } else {
        result[[key]] <- c(result[[key]], value)
      }
    } else {
      result[[key]] <- value
    }
  }

  return(result)
}

#' Update Duration Save
#'
#' @description
#' Updates the duration_save value for a specific survey response using the
#' provided database connection pool.
#'
#' @param db_ops Database operations object instance
#' @param db_config List containing database configuration including:
#'   \itemize{
#'     \item write_table: Table name for survey data
#'   }
#' @param session_id Character string containing the Shiny session token
#' @param duration_save Numeric value of the duration to save
#' @param logger Logger object for recording operations
#'
#' @return NULL invisibly
#'
#' @keywords internal
update_duration_save <- function(db_ops, db_config, session_id, duration_save, logger) {
  # Get the row ID using the existing connection
  row_id <- db_ops$read_table(
    db_config$write_table,
    columns = "id",
    filters = list(session_id = session_id),
    order_by = "id",
    desc = TRUE,
    limit = 1
  )$id

  tryCatch({
    # Perform the update using the existing db_ops instance
    db_ops$update_by_id(
      db_config$write_table,
      row_id,
      list(duration_save = duration_save)
    )

    logger$log_message(
      sprintf("Updated duration_save for row %d", row_id),
      "INFO",
      "DATABASE"
    )
  },
  error = function(e) {
    logger$log_message(
      sprintf("Failed to update duration_save: %s", e$message),
      "ERROR",
      "DATABASE"
    )
  })

  invisible(NULL)
}
