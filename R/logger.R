#' Shiny App Logger Class
#'
#' An R6 class that provides queued logging functionality for Shiny
#' applications. This class efficiently handles logging to a PostgreSQL
#' database using a message queue and connection pooling.
#'
#' @description
#' Creates a new logger instance that queues messages and periodically writes them
#' to a PostgreSQL database while providing immediate console feedback.
#'
#' @details
#' The logger uses a queue-based approach where messages are:
#' 1. Added to an in-memory queue immediately
#' 2. Displayed in the console (if not suppressed)
#' 3. Periodically processed and written to the database
#'
#' This approach provides several benefits:
#' * Reliable database operations using the main connection pool
#' * Immediate console feedback for debugging
#' * Efficient batched database writes
#' * No cross-process connection issues
#'
#' @section Message Types:
#' Supported message types with distinct visual styles:
#' * `INFO`: Regular informational messages (green)
#' * `WARN`: Warning messages (yellow)
#' * `ERROR`: Error messages (red)
#'
#' @section Public Fields:
#' * `log_table`: Character. Database table name for logging
#' * `session_id`: Character. Unique identifier for current session
#' * `survey_name`: Character. Name of the survey being logged
#' * `suppress_logs`: Logical. Whether to suppress console output
#' * `queue`: data.frame. Internal message queue for batch processing
#'
#' @section Methods:
#' \describe{
#'   \item{`initialize(log_table, session_id, survey_name, suppress_logs = FALSE)`}{
#'     Creates a new logger instance and initializes the logging table if needed.
#'   }
#'   \item{`log_message(message, type = "INFO", zone = "DEFAULT")`}{
#'     Queues a message for logging and displays it in the console.
#'   }
#' }
#'
#' @section Private Methods:
#' \describe{
#'   \item{`process_queue()`}{
#'     Processes queued messages and writes them to the database.
#'   }
#'   \item{`start_logging_observer()`}{
#'     Starts the background observer that processes the queue.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' # Initialize logger with console output
#' logger <- survey_logger$new(
#'   log_table = "survey_app_logs",
#'   session_id = "user123",
#'   survey_name = "customer_feedback"
#' )
#'
#' # Log different types of messages
#' logger$log_message("Survey started", "INFO", "initialization")
#' logger$log_message("Missing optional field", "WARN", "validation")
#' logger$log_message("Database error", "ERROR", "data")
#' }
#'
#' @import R6
#' @importFrom DBI dbExecute dbConnect dbDisconnect dbExistsTable
#' @importFrom pool poolCheckout poolReturn
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom shiny observe invalidateLater
#'
#' @export
survey_logger <- R6::R6Class(
  "survey_logger",
  
  public = list(
    #' @field log_table Character string specifying the name of the PostgreSQL table where logs will be stored.
    log_table = NULL,
    
    #' @field session_id Character string containing a unique identifier for the current Shiny session.
    session_id = NULL,
    
    #' @field survey_name Character string identifying which survey is being logged.
    survey_name = NULL,
    
    #' @field suppress_logs Logical flag indicating whether to suppress console output messages.
    suppress_logs = NULL,
    
    #' @field queue data.frame containing queued messages waiting to be written to the database.
    queue = NULL,
    
    #' Initialize a new survey logger instance
    #'
    #' Creates a new logger instance, initializes the message queue, and ensures
    #' the logging table exists in the database.
    #'
    #' @param log_table Character string specifying the name of the logging table
    #' @param session_id Character string containing a unique session identifier
    #' @param survey_name Character string identifying the survey
    #' @param suppress_logs Logical flag to suppress console output. Default: FALSE
    #'
    #' @return A new survey_logger instance (invisible)
    initialize = function(log_table, session_id, survey_name, suppress_logs = FALSE) {
      self$log_table <- log_table
      self$session_id <- session_id
      self$survey_name <- survey_name
      self$suppress_logs <- suppress_logs
      
      # Initialize empty queue
      self$queue <- data.frame(
        session_id = character(),
        survey_name = character(),
        timestamp = as.POSIXct(character()),
        zone = character(),
        message = character(),
        type = character(),
        stringsAsFactors = FALSE
      )
      
      if (!self$suppress_logs) {
        cli::cli_alert_success("[Session {.val {self$session_id}}] {.field INFO}: Started logger")
      }
      
      # Create table if it doesn't exist
      pool <- get("app_pool", envir = .GlobalEnv)
      conn <- pool::poolCheckout(pool)
      on.exit(pool::poolReturn(conn))
      
      if (!DBI::dbExistsTable(conn, self$log_table)) {
        query <- sprintf(
          "CREATE TABLE %s (
            id SERIAL PRIMARY KEY,
            session_id TEXT,
            survey_name TEXT,
            timestamp TIMESTAMP WITH TIME ZONE,
            zone TEXT,
            message TEXT,
            type TEXT
          )",
          self$log_table
        )
        DBI::dbExecute(conn, query)
        if (!self$suppress_logs) {
          cli::cli_alert_success("[Session {.val {self$session_id}}] {.field INFO}: Created logger table")
        }
      }
      
      # Start the logging observer
      private$start_logging_observer()
    },
    
    #' Queue a message for logging
    #'
    #' Adds a message to the logging queue and displays it immediately in the console
    #' if console output is not suppressed.
    #'
    #' @param message Character string containing the message to log
    #' @param type Character string specifying message type. Must be one of:
    #'   * "INFO": Regular informational messages (green)
    #'   * "WARN": Warning messages (yellow)
    #'   * "ERROR": Error messages (red)
    #'   Default: "INFO"
    #' @param zone Character string identifying the logging zone for message
    #'   categorization. Default: "DEFAULT"
    #'
    #' @return NULL invisibly
    log_message = function(message, type = "INFO", zone = "DEFAULT") {
      # Add message to queue
      new_message <- data.frame(
        session_id = self$session_id,
        survey_name = self$survey_name,
        timestamp = Sys.time(),
        zone = zone,
        message = message,
        type = type,
        stringsAsFactors = FALSE
      )
      
      # Add to queue
      self$queue <- rbind(self$queue, new_message)
      
      # Display message immediately
      if (!self$suppress_logs) {
        switch(type,
               "ERROR" = cli::cli_alert_danger(
                 "[Session {.val {self$session_id}}] {.field {type}}: {message}",
                 .envir = environment()
               ),
               "WARN" = cli::cli_alert_warning(
                 "[Session {.val {self$session_id}}] {.field {type}}: {message}",
                 .envir = environment()
               ),
               cli::cli_alert_success(
                 "[Session {.val {self$session_id}}] {.field {type}}: {message}",
                 .envir = environment()
               )
        )
      }
      
      invisible(NULL)
    }
  ),
  
  private = list(
    #' Process the message queue
    #'
    #' Internal method that processes queued messages and writes them to the database
    #' using the main connection pool.
    #'
    #' @keywords internal
    process_queue = function() {
      if (nrow(self$queue) == 0) return()
      
      # Get the global connection pool
      pool <- get("app_pool", envir = .GlobalEnv)
      
      tryCatch({
        conn <- pool::poolCheckout(pool)
        on.exit(pool::poolReturn(conn))
        
        # Prepare the query
        query <- sprintf(
          "INSERT INTO %s (session_id, survey_name, timestamp, zone, message, type)
           VALUES ($1, $2, $3, $4, $5, $6)",
          self$log_table
        )
        
        # Process each message in the queue
        for (i in 1:nrow(self$queue)) {
          row <- self$queue[i, ]
          DBI::dbExecute(
            conn,
            query,
            params = list(
              row$session_id,
              row$survey_name,
              row$timestamp,
              row$zone,
              row$message,
              row$type
            )
          )
        }
        
        # Clear the queue after successful processing
        self$queue <- self$queue[0, ]
        
      }, error = function(e) {
        if (!self$suppress_logs) {
          message(sprintf("Error processing log queue: %s", e$message))
        }
      })
    },
    
    #' Start the logging observer
    #'
    #' Internal method that starts a background observer to periodically process
    #' the message queue.
    #'
    #' @keywords internal
    start_logging_observer = function() {
      # Create an observer that processes the queue periodically
      observe({
        invalidateLater(1000)  # Check queue every second
        private$process_queue()
      })
    }
  )
)