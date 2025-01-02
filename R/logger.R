#' @title Survey Response Logger
#' @description An R6 class that provides asynchronous logging functionality for survey responses
#' @format An R6 class object
#' @details This class handles asynchronous logging of survey responses to a PostgreSQL database.
#' It uses connection pooling and futures for efficient database operations. The class maintains
#' a single logging table per instance and handles all database connections internally.
#'
#' @section Public Fields:
#' \describe{
#'   \item{log_table}{character. Name of the database table for logging}
#'   \item{session_id}{character. Unique identifier for the current session}
#'   \item{survey_name}{character. Name of the survey being logged}
#'   \item{db_params}{list. Database connection parameters}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(log_table, session_id, survey_name, db_config = NULL)}{
#'     Creates a new logger instance
#'     \describe{
#'       \item{log_table}{character. Name of the logging table}
#'       \item{session_id}{character. Unique session identifier}
#'       \item{survey_name}{character. Name of the survey}
#'       \item{db_config}{list. Optional database configuration parameters}
#'     }
#'   }
#'   \item{log_message(message, type = "INFO", zone = "DEFAULT")}{
#'     Logs a message asynchronously
#'     \describe{
#'       \item{message}{character. Message to log}
#'       \item{type}{character. Type of message (e.g., "INFO", "ERROR")}
#'       \item{zone}{character. Zone identifier for message categorization}
#'     }
#'   }
#' }
#'
#' @section Private Methods:
#' \describe{
#'   \item{ensure_table_exists()}{
#'     Creates the logging table if it doesn't exist. Uses the global connection pool
#'     to execute the CREATE TABLE IF NOT EXISTS query.
#'   }
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom promises future_promise then catch
#' @importFrom future future
#' @importFrom DBI dbExecute dbConnect dbDisconnect
#' @importFrom RPostgres Postgres
#' @importFrom pool poolCheckout poolReturn
#'
#' @examples
#' \dontrun{
#' # Initialize logger
#' logger <- survey_logger$new(
#'   log_table = "survey_logs",
#'   session_id = "user123",
#'   survey_name = "customer_feedback"
#' )
#'
#' # Log different types of messages
#' logger$log_message("Survey started", "INFO", "initialization")
#' logger$log_message("Question 1 completed", "INFO", "progress")
#' logger$log_message("Invalid response format", "ERROR", "validation")
#' }
#'
#' @export
survey_logger <- R6::R6Class(
  "survey_logger",
  public = list(
    #' @field log_table Name of the database table for logging
    log_table = NULL,

    #' @field session_id Unique identifier for the current session
    session_id = NULL,

    #' @field survey_name Name of the survey being logged
    survey_name = NULL,

    #' @field db_params List of database connection parameters
    db_params = NULL,

    #' @description Initialize a new survey logger instance
    #' @param log_table character. Name of the logging table
    #' @param session_id character. Unique session identifier
    #' @param survey_name character. Name of the survey
    initialize = function(log_table, session_id, survey_name) {
      self$log_table <- log_table
      self$session_id <- session_id
      self$survey_name <- survey_name
      self$db_params <- list(
        host = Sys.getenv("HOST"),
        port = as.numeric(Sys.getenv("PORT")),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("USER"),
        password = Sys.getenv("PASSWORD")
      )
      private$ensure_table_exists()
    },

    #' @description Log a message asynchronously to the database
    #' @param message character. Message to log
    #' @param type character. Type of message (default: "INFO")
    #' @param zone character. Zone identifier for message categorization (default: "DEFAULT")
    #' @return invisible(NULL)
    log_message = function(message, type = "INFO", zone = "DEFAULT") {
      db_params <- self$db_params
      log_table <- self$log_table
      session_id <- self$session_id
      survey_name <- self$survey_name

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
            message(sprintf("[Session %s] Logged: %s", session_id, message))
            invisible(value)
          }
        ) |>
        promises::catch(
          function(error) {
            warning(sprintf("[Session %s] Logging error: %s", session_id, error$message))
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

      query <- sprintf(
        "CREATE TABLE IF NOT EXISTS %s (
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
      suppressMessages(DBI::dbExecute(conn, query))
    }
  )
)
