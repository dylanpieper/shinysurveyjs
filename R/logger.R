#' Survey Logger Class
#'
#' @description
#' R6 Class providing dual logging functionality for Shiny survey applications:
#' console messaging and database logging of survey metadata.
#'
#' @details
#' The logger provides two distinct logging mechanisms:
#'
#' **Console Logging (`log_message`)**:
#' * Immediate color-coded console output for app messages, warnings, and errors
#' * No database persistence - console only
#' * Supports custom zones/contexts for message categorization
#'
#' **Database Logging (`log_entry`)**:
#' * Queued logging of survey metadata to database table
#' * Tracks survey completion metrics (timing, errors, SQL statements)
#' * Uses batch processing with automatic queue management
#' * Only logs database errors after survey is loaded (conditional logging)
#'
#' Key Features:
#' * Dual-purpose logging: console messages + database survey metadata
#' * Uses main application connection for database operations
#' * Efficient batch database writes with automatic queue processing
#' * Immediate console feedback with color coding and zone support
#' * Database connection health monitoring
#' * SQL statement tracking for database error debugging
#'
#' @section Console Message Types:
#' * `INFO`: Regular informational messages (green console output)
#' * `WARN`: Warning messages (yellow console output)
#' * `ERROR`: Error messages (red console output)
#'
#' @import R6
#' @importFrom DBI dbExecute dbExistsTable
#' @importFrom pool poolCheckout poolReturn
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom shiny observe invalidateLater
#'
#' @export
survey_logger <- R6::R6Class(
  "survey_logger",
  public = list(
    #' @field log_table Character string specifying the database table for logs
    log_table = NULL,

    #' @field session_id Character string containing unique session identifier
    session_id = NULL,

    #' @field table_name Character string identifying the current table
    table_name = NULL,

    #' @field survey_loaded Logical flag indicating if a survey has been successfully loaded
    survey_loaded = NULL,

    #' @field suppress_logs Logical flag to suppress console output
    suppress_logs = NULL,

    #' @field queue data.frame containing queued messages for batch processing
    queue = NULL,

    #' @field last_sql Character string containing the last executed SQL statement
    last_sql = NULL,

    #' @description
    #' Initialize a new survey logger instance
    #'
    #' @param log_table Character string specifying logging table name
    #' @param session_id Character string containing session identifier
    #' @param table_name Character string identifying the table
    #' @param echo Logical flag to display console output. Default: TRUE
    initialize = function(log_table, session_id, table_name, echo = TRUE) {
      # Ensure log_table is not NULL or empty
      if (is.null(log_table) || length(log_table) == 0 || log_table == "") {
        log_table <- "sjs_logs"
      }
      
      self$log_table <- log_table
      self$session_id <- session_id
      self$table_name <- table_name
      self$survey_loaded <- FALSE
      self$suppress_logs <- !echo
      self$last_sql <- NULL

      # Initialize empty queue
      self$queue <- data.frame(
        table_name = character(),
        table_id = integer(),
        sql = character(),
        message = character(),
        duration_load = numeric(),
        duration_complete = numeric(),
        duration_save = numeric(),
        ip_address = character(),
        created_at = as.POSIXct(character()),
        stringsAsFactors = FALSE
      )

      if (!self$suppress_logs) {
        cli::cli_alert_success("Started logger for session {.val {self$session_id}}")
      }

      # Initialize logging table using global app connection
      private$initialize_table()

      # Start queue processing
      private$start_queue_processor()
    },

    #' @description
    #' Update the table name for this logger instance
    #'
    #' @param table_name Character string identifying the new table name
    #'
    #' @return Invisible NULL
    update_table_name = function(table_name) {
      self$table_name <- table_name
      invisible(NULL)
    },

    #' @description
    #' Mark the survey as loaded to enable error logging
    #'
    #' @return Invisible NULL
    mark_survey_loaded = function() {
      self$survey_loaded <- TRUE
      invisible(NULL)
    },

    #' @description
    #' Update the last SQL statement executed
    #'
    #' @param sql Character string containing the SQL statement
    #'
    #' @return Invisible NULL
    update_last_sql = function(sql) {
      self$last_sql <- sql
      invisible(NULL)
    },

    #' @description
    #' Queue a log entry for database insert (only for loaded surveys)
    #'
    #' @param table_id Integer ID from the table
    #' @param message Character string containing error message (only for DB errors after survey loaded)
    #' @param ip_address Character string containing client IP address
    #' @param duration_load Numeric time spent loading (seconds)
    #' @param duration_complete Numeric time spent completing survey (seconds)
    #' @param duration_save Numeric time spent saving (seconds)
    #' @param sql Character string containing the SQL that failed (only for errors)
    #' @param force_log Logical flag to force logging even if survey not loaded (internal use)
    #'
    #' @return Invisible NULL
    log_entry = function(table_id, message = NULL, ip_address = NULL,
                         duration_load = NULL, duration_complete = NULL,
                         duration_save = NULL, sql = NULL, force_log = FALSE) {
      # Only log errors if survey is loaded, unless forced
      if (!is.null(message) && !self$survey_loaded && !force_log) {
        return(invisible(NULL))
      }
      # Ensure all values are single length
      table_id <- if (is.null(table_id) || length(table_id) == 0) NA else table_id[1]
      message <- if (is.null(message) || length(message) == 0) NA else message[1]
      ip_address <- if (is.null(ip_address) || length(ip_address) == 0) NA else ip_address[1]
      duration_load <- if (is.null(duration_load) || length(duration_load) == 0) NA else duration_load[1]
      duration_complete <- if (is.null(duration_complete) || length(duration_complete) == 0) NA else duration_complete[1]
      duration_save <- if (is.null(duration_save) || length(duration_save) == 0) NA else duration_save[1]

      # Use provided sql or fall back to last_sql
      if (is.null(sql) || length(sql) == 0) {
        sql <- self$last_sql
      } else {
        sql <- sql[1]
      }
      sql <- if (is.null(sql) || length(sql) == 0) NA else sql

      # Add entry to queue
      new_entry <- data.frame(
        table_name = self$table_name,
        table_id = table_id,
        sql = sql,
        message = message,
        duration_load = duration_load,
        duration_complete = duration_complete,
        duration_save = duration_save,
        ip_address = ip_address,
        created_at = Sys.time(),
        stringsAsFactors = FALSE
      )

      self$queue <- rbind(self$queue, new_entry)

      # Display DB errors in console if not suppressed
      if (!self$suppress_logs && !is.null(message) && !is.na(message)) {
        cli::cli_alert_danger(
          "[DATABASE ERROR] {message}"
        )
        if (!is.null(sql) && !is.na(sql)) {
          cli::cli_alert_danger("SQL: {sql}")
        }
      }

      invisible(NULL)
    },

    #' @description
    #' Helper method for simple console logging (no database)
    #' @param message Character string containing message to log
    #' @param type Character string specifying message type (INFO/WARN/ERROR)
    #' @param zone Character string specifying the logging zone/context (default: "DEFAULT")
    #' @return Invisible NULL
    log_message = function(message, type = "INFO", zone = "DEFAULT") {
      # Only display in console if not suppressed
      if (!self$suppress_logs) {
        switch(type,
          "ERROR" = cli::cli_alert_danger(
            "[{.val {zone}}] {message}",
            .envir = environment()
          ),
          "WARN" = cli::cli_alert_warning(
            "[{.val {zone}}] {message}",
            .envir = environment()
          ),
          cli::cli_alert_success(
            "[{.val {zone}}] {message}",
            .envir = environment()
          )
        )
      }
      invisible(NULL)
    }
  ),
  private = list(
    initialize_table = function() {
      conn <- get("app_pool", envir = .GlobalEnv)
      tryCatch(
        {
          if (!DBI::dbExistsTable(conn, self$log_table)) {
            query <- sprintf(
              "
            CREATE TABLE IF NOT EXISTS %s (
              id INT AUTO_INCREMENT PRIMARY KEY,
              table_name TEXT NOT NULL,
              table_id INT,
              sql TEXT,
              message TEXT,
              duration_load DECIMAL(10,3),
              duration_complete DECIMAL(10,3),
              duration_save DECIMAL(10,3),
              ip_address TEXT,
              created_at TIMESTAMP NOT NULL
            )",
              self$log_table
            )
            DBI::dbExecute(conn, query)

            if (!self$suppress_logs) {
              cli::cli_alert_success("Created logging table {.val {self$log_table}}")
            }
          }
        },
        error = function(e) {
          if (!self$suppress_logs) {
            cli::cli_alert_danger("Failed to initialize logging table: {e$message}")
          }
        }
      )
    },
    process_queue = function() {
      if (nrow(self$queue) == 0) {
        return(invisible(NULL))
      }

      conn <- get("app_pool", envir = .GlobalEnv)
      tryCatch(
        {
          query <- sprintf(
            "
          INSERT INTO %s
            (table_name, table_id, sql, message,
             duration_load, duration_complete, duration_save, ip_address, created_at)
          VALUES
            (?, ?, ?, ?, ?, ?, ?, ?, ?)",
            self$log_table
          )

          # Process queue in batches
          for (i in seq_len(nrow(self$queue))) {
            row <- self$queue[i, ]
            DBI::dbExecute(
              conn,
              query,
              params = list(
                row$table_name,
                if (is.na(row$table_id) || is.null(row$table_id)) NA else row$table_id,
                if (is.na(row$sql) || is.null(row$sql)) NA else row$sql,
                if (is.na(row$message) || is.null(row$message)) NA else row$message,
                if (is.na(row$duration_load) || is.null(row$duration_load)) NA else row$duration_load,
                if (is.na(row$duration_complete) || is.null(row$duration_complete)) NA else row$duration_complete,
                if (is.na(row$duration_save) || is.null(row$duration_save)) NA else row$duration_save,
                if (is.na(row$ip_address) || is.null(row$ip_address)) NA else row$ip_address,
                row$created_at
              )
            )
          }

          # Clear processed messages
          self$queue <- self$queue[0, ]
        },
        error = function(e) {
          if (!self$suppress_logs) {
            cli::cli_alert_danger("Failed to process message queue: {e$message}")
          }
        }
      )

      invisible(NULL)
    },
    start_queue_processor = function() {
      observe({
        invalidateLater(1000) # Process queue every second
        private$process_queue()
      })
    }
  )
)
