#' Survey Logger Class
#'
#' @description
#' R6 Class providing queued logging functionality for Shiny survey applications
#' using the main application's database connection pool.
#'
#' @details
#' The logger uses a queue-based approach where messages are:
#' 1. Added to an in-memory queue immediately
#' 2. Displayed in the console (if not suppressed)
#' 3. Periodically processed and written to the database in batches
#'
#' Features:
#' * Uses main application pool for database operations
#' * Efficient batch database writes
#' * Immediate console feedback with color coding
#' * Connection pool health monitoring
#'
#' @section Message Types:
#' * `INFO`: Regular informational messages (green)
#' * `WARN`: Warning messages (yellow)
#' * `ERROR`: Error messages (red)
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
    #' @field log_table Character string specifying the PostgreSQL table for logs
    log_table = NULL,
    
    #' @field session_id Character string containing unique session identifier
    session_id = NULL,
    
    #' @field survey_name Character string identifying the current survey
    survey_name = NULL,
    
    #' @field suppress_logs Logical flag to suppress console output
    suppress_logs = NULL,
    
    #' @field queue data.frame containing queued messages for batch processing
    queue = NULL,
    
    #' @description
    #' Initialize a new survey logger instance
    #'
    #' @param log_table Character string specifying logging table name
    #' @param session_id Character string containing session identifier
    #' @param survey_name Character string identifying the survey
    #' @param suppress_logs Logical flag to suppress console output. Default: FALSE
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
        cli::cli_alert_success("Started logger for session {.val {self$session_id}}")
      }
      
      # Initialize logging table using global app pool
      private$initialize_table()
      
      # Start queue processing
      private$start_queue_processor()
    },
    
    #' @description
    #' Queue a message for logging and display in console
    #'
    #' @param message Character string containing message to log
    #' @param type Character string specifying message type (INFO/WARN/ERROR)
    #' @param zone Character string identifying logging zone. Default: "DEFAULT"
    #'
    #' @return Invisible NULL
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
      
      self$queue <- rbind(self$queue, new_message)
      
      # Display in console if not suppressed
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
      pool <- get("app_pool", envir = .GlobalEnv)
      conn <- NULL
      tryCatch({
        conn <- pool::poolCheckout(pool)
        
        if (!DBI::dbExistsTable(conn, self$log_table)) {
          query <- sprintf("
            CREATE TABLE IF NOT EXISTS %s (
              id SERIAL PRIMARY KEY,
              session_id TEXT NOT NULL,
              survey_name TEXT NOT NULL,
              timestamp TIMESTAMP WITH TIME ZONE NOT NULL,
              zone TEXT NOT NULL,
              message TEXT NOT NULL,
              type TEXT NOT NULL
            )",
                           self$log_table
          )
          DBI::dbExecute(conn, query)
          
          if (!self$suppress_logs) {
            cli::cli_alert_success("Created logging table {.val {self$log_table}}")
          }
        }
      }, error = function(e) {
        if (!self$suppress_logs) {
          cli::cli_alert_danger("Failed to initialize logging table: {e$message}")
        }
      }, finally = {
        if (!is.null(conn)) {
          pool::poolReturn(conn)
        }
      })
    },
    
    process_queue = function() {
      if (nrow(self$queue) == 0) return(invisible(NULL))
      
      pool <- get("app_pool", envir = .GlobalEnv)
      conn <- NULL
      tryCatch({
        conn <- pool::poolCheckout(pool)
        
        query <- sprintf("
          INSERT INTO %s 
            (session_id, survey_name, timestamp, zone, message, type)
          VALUES 
            ($1, $2, $3, $4, $5, $6)",
                         self$log_table
        )
        
        # Process queue in batches
        for (i in seq_len(nrow(self$queue))) {
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
        
        # Clear processed messages
        self$queue <- self$queue[0, ]
        
      }, error = function(e) {
        if (!self$suppress_logs) {
          cli::cli_alert_danger("Failed to process message queue: {e$message}")
        }
      }, finally = {
        if (!is.null(conn)) {
          pool::poolReturn(conn)
        }
      })
      
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