#' Shiny App Logger Class
#'
#' An R6 class that provides asynchronous logging functionality for Shiny
#' application messages. This class efficiently handles logging to a PostgreSQL
#' database using connection pooling and futures for non-blocking database operations.
#'
#' @format An [R6][R6::R6Class] class object.
#'
#' @description
#' Creates a new logger instance for recording application messages asynchronously
#' to a PostgreSQL database with visual console feedback.
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
#' * `db_params`: List. Database connection parameters
#' * `suppress_logs`: Logical. Whether to suppress console output
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
#' @importFrom promises future_promise then catch
#' @importFrom future future
#' @importFrom DBI dbExecute dbConnect dbDisconnect dbExistsTable
#' @importFrom RPostgres Postgres
#' @importFrom pool poolCheckout poolReturn
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_warning
survey_logger <- R6::R6Class(
  "survey_logger",
  public = list(
    #' @field log_table Character string specifying the name of the PostgreSQL table where logs will be stored.
    log_table = NULL,

    #' @field session_id Character string containing a unique identifier for the current Shiny session.
    session_id = NULL,

    #' @field survey_name Character string identifying which survey is being logged.
    survey_name = NULL,

    #' @field db_params List containing PostgreSQL connection parameters including host, port, database name,
    #' username and password.
    db_params = NULL,

    #' @field suppress_logs Logical flag indicating whether to suppress console output messages. When TRUE,
    #' messages are only logged to the database without console feedback.
    suppress_logs = NULL,

    #' Initialize a new survey logger instance
    #'
    #' Creates a new logger instance and ensures the logging table exists in the database.
    #' Connection parameters are read from environment variables if not explicitly provided.
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
      self$db_params <- list(
        host = Sys.getenv("HOST"),
        port = as.numeric(Sys.getenv("PORT")),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("USER"),
        password = Sys.getenv("PASSWORD")
      )

      if (!self$suppress_logs) {
        cli::cli_alert_success("[Session {.val {self$session_id}}] {.field INFO}: Started logger")
      }
      private$ensure_table_exists()
    },

    #' Log a message asynchronously
    #'
    #' Records a message to the PostgreSQL database asynchronously using futures and
    #' provides visual console feedback based on message type. Messages are stored with
    #' timestamp, session ID, and zone information.
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
    #' @return NULL invisibly. Operation happens asynchronously.
    log_message = function(message, type = "INFO", zone = "DEFAULT") {
      db_params <- self$db_params
      log_table <- self$log_table
      session_id <- self$session_id
      survey_name <- self$survey_name
      suppress_logs <- self$suppress_logs

      promises::future_promise({
        conn <- do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), db_params))
        on.exit(DBI::dbDisconnect(conn))

        timestamp <- Sys.time()
        query <- sprintf(
          "INSERT INTO %s (session_id, survey_name, timestamp, zone, message, type)
           VALUES ($1, $2, $3, $4, $5, $6)",
          log_table
        )

        DBI::dbExecute(
          conn,
          query,
          params = list(
            session_id,
            survey_name,
            timestamp,
            zone,
            message,
            type
          )
        )
      }) |>
        promises::then(
          onFulfilled = function(value) {
            if (!suppress_logs) {
              switch(type,
                     "ERROR" = cli::cli_alert_danger(
                       "[Session {.val {session_id}}] {.field {type}}: {message}",
                       .envir = environment()
                     ),
                     "WARN" = cli::cli_alert_warning(
                       "[Session {.val {session_id}}] {.field {type}}: {message}",
                       .envir = environment()
                     ),
                     cli::cli_alert_success(
                       "[Session {.val {session_id}}] {.field {type}}: {message}",
                       .envir = environment()
                     )
              )
            }
            invisible(value)
          }
        ) |>
        promises::catch(
          function(error) {
            if (!suppress_logs) {
              cli::cli_alert_danger(
                "[Session {.val {session_id}}] Logging error: {.error {error$message}}",
                .envir = environment()
              )
            }
            NULL
          }
        )
    }
  ),
  private = list(
    ensure_table_exists = function() {
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
    }
  )
)
