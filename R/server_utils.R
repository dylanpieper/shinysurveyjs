#' Setup Global Survey Environment and Database Connection
#'
#' Setup the global survey environment by creating database connections,
#' environment variables, and a future asynchronous processing plan. Validates
#' database configuration, sets environment variables if needed, configures Shiny
#' settings, establishes a database connection pool, and initializes a future plan.
#'
#' @param db_config List. Database configuration parameters:
#'   * `host`: Database host address
#'   * `port`: Database port number
#'   * `db_name`: Name of the database
#'   * `user`: Database username
#'   * `password`: Database password
#' @param shiny_config List. Optional Shiny configuration parameters to pass to
#'   `configure_shiny()`. Applied before database initialization.
#'
#' @details
#' The function:
#' * Checks required database fields
#' * Sets environment variables if missing (HOST, PORT, DB_NAME, USER, PASSWORD)
#' * Applies optional Shiny settings
#' * Creates global database connection if needed
#'
#' The database connection is stored globally as 'app_conn' and reused if it exists.
#' Environment variables are only set if not already present.
#'
#' @return Invisibly returns the initialized database connection object
#'
#' @importFrom cli cli_h1 cli_alert_danger cli_alert_success cli_alert_info
#'   cli_alert_warning
#'
#' @noRd
#' @keywords internal
survey_setup <- function(db_config, shiny_config = NULL, is_multisurvey = FALSE) {
  # Start status group for setup process
  cli::cli_h1("Initializing Survey Environment")

  # Validate write_table parameter (skip for multisurvey mode)
  if (!is_multisurvey && (is.null(db_config$write_table) ||
    !is.character(db_config$write_table) ||
    nchar(db_config$write_table) == 0)) {
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

  # Initialize global database connection with error handling
  cli::cli_h2("Database Connection")
  if (!exists("app_conn", envir = .GlobalEnv)) {
    tryCatch(
      {
        # Handle driver parameter - create driver object if needed
        if (is.null(db_config$driver)) {
          db_config$driver <- RMariaDB::MariaDB()
        }
        assign("app_conn", do.call(
          db_conn_open,
          db_config[c("driver", "host", "port", "db_name", "user", "password")]
        ),
        envir = .GlobalEnv
        )
        cli::cli_alert_success(paste("Started database connection for", class(db_config$driver)[1]))
      },
      error = function(e) {
        cli::cli_alert_danger("Failed to initialize database connection: {e$message}")
        stop("Database connection initialization failed")
      }
    )
  } else {
    cli::cli_alert_success("Using existing database connection")
  }

  # Return connection object invisibly
  invisible(get("app_conn", envir = .GlobalEnv))
}

#' Configure Shiny App Settings
#'
#' Sets global Shiny options by automatically adding the 'shiny.' prefix to option names.
#' Validates and processes option values using type-specific handlers.
#'
#' @param ... Named arguments passed as Shiny options. Names will be prefixed with 'shiny.'.
#' @param type_handlers Named list of functions for option value processing. Default handlers
#'   provided for:
#'   * numeric: Validates numeric values
#'   * logical: Converts to TRUE/FALSE
#'   * character: Processes string values
#'
#' @return Invisible NULL. Modifies global Shiny options.
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
#' @noRd
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
#' Setup the server-side components of the survey application by initializing logging and
#' database operations in the parent environment. Creates a logger instance and establishes
#' database connections with error handling.
#'
#' @param session Shiny session object containing the session token.
#' @param db_config List. Database configuration parameters:
#'   * `log_table`: Name of logging table
#' @param app_conn Database connection object from global environment
#' @param survey_logger Reference class object for logging functionality
#' @param db_ops Reference class object for database operations
#'
#' @details
#' Creates two objects in the parent environment:
#' * `logger`: Initialized survey logger object
#' * `db_ops`: Initialized database operations object (NULL if initialization fails)
#'
#' @examples
#' \dontrun{
#' server_setup(
#'   session = session,
#'   db_config = db_config,
#'   app_conn = app_conn,
#'   survey_logger = survey_logger,
#'   db_ops = db_ops,
#'   suppress_logs = FALSE
#' )
#' }
#'
#' @noRd
#' @keywords internal
server_setup <- function(session, db_config, app_conn, survey_logger, db_ops, echo, is_multisurvey = FALSE) {
  # Initialize survey app logger
  survey_name <- if (is_multisurvey) "multisurvey" else db_config$write_table
  logger <- survey_logger$new(
    log_table = db_config$log_table,
    session_id = session$token,
    survey_name = survey_name,
    echo = echo
  )

  # Only log session start for single survey mode or when survey is selected
  if (!is_multisurvey) {
    logger$log_message("Started session", zone = "SURVEY")
  }

  # Initialize database operations with error logging
  db_operations <- tryCatch(
    {
      db_ops$new(app_conn, logger)
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
#' Sets up server-side logic for displaying survey responses in a themed DataTable.
#' Handles rendering, visibility control, and theme-based styling.
#'
#' @param output Shiny output object.
#' @param rv Reactive values object containing:
#'   * `survey_completed`: Boolean indicating survey completion
#'   * `loading`: Boolean for loading state
#'   * `survey_responses`: Data frame of responses
#'   * `error_message`: Error message string if any
#' @param show_response Logical. Display response table. Default: `FALSE`.
#' @param theme_mode String. Color mode, either "light" or "dark". Default: "light".
#' @param theme_color String. Hex color code for primary theme.
#'
#' @details
#' Creates two reactive outputs:
#'   * `surveyResponseTable`: Themed DataTable of responses
#'   * `showResponseTable`: Visibility control
#'
#' Table displays when:
#'   * Survey is completed
#'   * Not loading
#'   * No errors present
#'   * `show_response` is `TRUE`
#'
#' Theme styling:
#'   * Light mode: White background, dark text
#'   * Dark mode: Dark background, light text
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
#' @noRd
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
      border = "#c0c0c0",
      header_bg = "#d5d5d5"
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
        dom = "t", # Show only the table, no controls
        ordering = FALSE, # Disable sorting
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
#' Handles cleanup tasks when a Shiny session ends by logging session termination
#' and closing the global database pool connection. This function should be called
#' within server initialization to ensure proper resource cleanup.
#'
#' @param session Shiny session object containing the session token.
#' @param logger Logger object with log_message method for event recording.
#' @param zone Character. Logging zone identifier. Default: "SURVEY"
#'
#' @details
#' This function:
#' 1. Registers a session end handler using onSessionEnded
#' 3. Releases the connection from the database pool
#' 2. Logs the session termination event
#'
#' The cleanup process is logged at each step to provide clear tracking of the
#' shutdown sequence.
#'
#' @return Invisible NULL
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   logger <- survey_logger$new(...)
#'   server_clean(session, logger)
#' }
#' }
#'
#' @noRd
#' @keywords internal
server_clean <- function(session, logger, zone = "SURVEY") {
  session$onSessionEnded(function() {
    logger$log_message("Ended session", zone = zone)
  })
  invisible(NULL)
}

#' Parse URL Query Parameters
#'
#' Extracts and decodes query parameters from a URL string or Shiny session,
#' returning a named list of parameter values. Handles URL encoding, empty values,
#' and multiple parameters with the same name.
#'
#' @param input Character string URL with query parameters, or a Shiny session object.
#'
#' @return Named list of decoded query parameters.
#'
#' @examples
#' # Parse from URL string
#' params <- parse_query("https://example.com/page?name=John&age=25")
#'
#' # Parse within Shiny server
#' \dontrun{
#' server <- function(input, output, session) {
#'   params <- parse_query(session)
#' }
#' }
#'
#' @noRd
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

# update_duration_save function removed - timing data now handled in log table
