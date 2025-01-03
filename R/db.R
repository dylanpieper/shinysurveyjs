#' Open Database Pool
#'
#' Creates and manages a global database pool connection using PostgreSQL.
#'
#' @param host Database host
#' @param port Database port
#' @param db_name Database name
#' @param user Database username
#' @param password Database password
#' @param min_size Minimum pool size (default: 1)
#' @param max_size Maximum pool size (default: Inf)
#'
#' @return A database pool object
#' @importFrom pool dbPool poolClose
#' @importFrom RPostgres Postgres
db_pool_open <- function(host = NULL,
                         port = NULL,
                         db_name = NULL,
                         user = NULL,
                         password = NULL,
                         min_size = 1,
                         max_size = Inf) {
  if (all(!is.null(c(host, port, db_name, user, password)))) {
    pool <- pool::dbPool(
      RPostgres::Postgres(),
      host = host,
      port = port,
      dbname = db_name,
      user = user,
      password = password,
      minSize = min_size,
      maxSize = max_size
    )
    return(pool)
  } else {
    stop("Database connection parameters are required")
  }
}

#' Close Database Pool
#'
#' Closes the database connection pool and performs cleanup operations
#' when the application is shutting down.
#'
#' @param session Shiny session object
#'
#' @importFrom shiny onStop
db_pool_close <- function(session) {
  shiny::onStop(function() {
    if (exists("app_pool", envir = .GlobalEnv)) {
      cleanup_pool(get("app_pool", envir = .GlobalEnv))
      rm(app_pool, envir = .GlobalEnv)
    }
  }, session)
}

#' Database Operations Class
#'
#' R6 Class for managing database operations related to survey data storage
#' and retrieval using PostgreSQL.
#'
#' @field session_id Character. Unique identifier for the current session
#' @field pool Pool object. Database connection pool
#' @field logger survey_logger object. Logger instance for tracking operations
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbExecute dbQuoteIdentifier dbGetQuery dbBegin dbCommit
#' @importFrom DBI dbRollback dbIsValid dbExistsTable dbWriteTable
#' @importFrom pool poolCheckout poolReturn
db_ops <- R6::R6Class(
  "Database Operations",
  public = list(
    session_id = NULL,
    pool = NULL,
    logger = NULL,

    #' @description Initialize a new Database Operations instance
    #' @param pool Pool object. Database connection pool
    #' @param session_id Character. Unique identifier for the current session
    #' @param logger survey_logger object. Logger instance for tracking operations
    initialize = function(pool, session_id, logger) {
      if (is.null(pool)) {
        logger$log_message("Database pool cannot be NULL", "ERROR", "DATABASE")
        stop()
      }
      self$pool <- pool
      self$session_id <- session_id
      self$logger <- logger
    },

    #' @description Create a new survey data table in the database
    #' @param write_table Character. Name of the table to create
    #' @param data data.frame. Data frame containing the schema for the new table
    #' @details Creates a new table with appropriate column types based on the input data frame.
    #' Also creates a timestamp trigger for tracking updates.
    create_survey_data_table = function(write_table, data) {
      if (!is.data.frame(data) || nrow(data) == 0) {
        self$logger$log_message("Invalid data: must be a non-empty data frame", "ERROR", "DATABASE")
        stop()
      }

      table_name <- private$sanitize_survey_table_name(write_table)

      self$execute_db_operation(function(conn) {
        if (!DBI::dbExistsTable(conn, table_name)) {
          col_defs <- private$generate_column_definitions(data)
          create_query <- sprintf(
            "CREATE TABLE %s (id SERIAL PRIMARY KEY, %s);",
            DBI::dbQuoteIdentifier(conn, table_name),
            paste(col_defs, collapse = ", ")
          )
          DBI::dbExecute(conn, create_query)

          trigger_query <- sprintf(
            "CREATE TRIGGER update_timestamp_trigger_%s
             BEFORE UPDATE ON %s
             FOR EACH ROW
             EXECUTE FUNCTION update_timestamp();",
            table_name,
            DBI::dbQuoteIdentifier(conn, table_name)
          )
          DBI::dbExecute(conn, trigger_query)

          self$logger$log_message(
            sprintf("Created new table '%s'", table_name),
            "INFO",
            "DATABASE"
          )
        }
      }, sprintf("Failed to create table '%s'", table_name))

      invisible(table_name)
    },

    #' @description Execute a database operation with transaction handling
    #' @param operation Function. The database operation to execute
    #' @param error_message Character. Message to display if operation fails
    #' @details Handles connection pooling, transaction management, and error handling.
    #' Ensures proper cleanup of database connections.
    execute_db_operation = function(operation, error_message) {
      if (is.null(self$pool)) {
        self$logger$log_message("Database pool is not initialized", "ERROR", "DATABASE")
        stop()
      }

      conn <- NULL
      tryCatch({
        conn <- pool::poolCheckout(self$pool)
        if (is.null(conn) || !DBI::dbIsValid(conn)) {
          self$logger$log_message("Failed to obtain valid database connection", "ERROR", "DATABASE")
          stop("Failed to obtain valid database connection")
        }

        DBI::dbBegin(conn)
        result <- operation(conn)
        DBI::dbCommit(conn)

        return(result)
      }, error = function(e) {
        if (!is.null(conn) && DBI::dbIsValid(conn)) {
          tryCatch(
            {
              DBI::dbRollback(conn)
              self$logger$log_message("Transaction rolled back", "WARNING", "DATABASE")
            },
            error = function(rollback_error) {
              self$logger$log_message(
                sprintf("Rollback error: %s", rollback_error$message),
                "ERROR",
                "DATABASE"
              )
            }
          )
        }
        self$logger$log_message(
          sprintf("%s: %s", error_message, e$message),
          "ERROR",
          "DATABASE"
        )
        stop(sprintf("%s: %s", error_message, e$message))
      }, finally = {
        if (!is.null(conn)) {
          tryCatch(
            {
              pool::poolReturn(conn)
            },
            error = function(return_error) {
              self$logger$log_message(
                sprintf("Error returning connection to pool: %s", return_error$message),
                "ERROR",
                "DATABASE"
              )
            }
          )
        }
      })
    },

    #' @description Update an existing survey data table with new data
    #' @param write_table Character. Name of the table to update
    #' @param data data.frame. Data frame containing the new data
    #' @details Updates an existing table with new data, adding new columns if necessary.
    #' Automatically determines appropriate PostgreSQL data types.
    update_survey_data_table = function(write_table, data) {
      if (is.null(write_table) || !is.character(write_table)) {
        self$logger$log_message("Invalid write_table parameter", "ERROR", "DATABASE")
        stop()
      }

      if (!is.data.frame(data) || nrow(data) == 0) {
        self$logger$log_message("Invalid data: must be a non-empty data frame", "ERROR", "DATABASE")
        stop()
      }

      table_name <- private$sanitize_survey_table_name(write_table)

      self$execute_db_operation(function(conn) {
        if (!DBI::dbExistsTable(conn, table_name)) {
          self$logger$log_message(
            sprintf("Table '%s' does not exist", table_name),
            "ERROR",
            "table_check"
          )
          stop(sprintf("Table '%s' does not exist", table_name))
        }

        cols_query <- sprintf(
          "SELECT column_name, data_type
           FROM information_schema.columns
           WHERE table_name = '%s';",
          table_name
        )
        existing_cols <- DBI::dbGetQuery(conn, cols_query)

        new_cols <- setdiff(names(data), existing_cols$column_name)

        for (col in new_cols) {
          col_type <- private$get_postgres_type(data[[col]])
          alter_query <- sprintf(
            "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;",
            DBI::dbQuoteIdentifier(conn, table_name),
            DBI::dbQuoteIdentifier(conn, col),
            col_type
          )
          DBI::dbExecute(conn, alter_query)
          self$logger$log_message(
            sprintf("Added column %s to '%s'", col, table_name),
            "INFO",
            "DATABASE"
          )
        }

        DBI::dbWriteTable(
          conn,
          name = table_name,
          value = data,
          append = TRUE,
          row.names = FALSE
        )

        self$logger$log_message(
          sprintf("Inserted data into '%s' (n = %d)", table_name, nrow(data)),
          "INFO",
          "DATABASE"
        )
      }, sprintf("Failed to update table '%s'", table_name))

      invisible(table_name)
    }
  ),

  private = list(
    sanitize_survey_table_name = function(name) {
      tolower(gsub("[^[:alnum:]]", "_", name))
    },

    generate_column_definitions = function(data) {
      vapply(names(data), function(col) {
        type <- private$get_postgres_type(data[[col]])
        sprintf(
          "%s %s",
          DBI::dbQuoteIdentifier(self$pool, col),
          type
        )
      }, character(1))
    },

    get_postgres_type = function(vector) {
      if (is.numeric(vector)) {
        if (all(vector == floor(vector), na.rm = TRUE)) {
          return("INTEGER")
        }
        return("NUMERIC")
      }
      if (is.logical(vector)) {
        return("BOOLEAN")
      }
      if (inherits(vector, "POSIXt")) {
        return("TIMESTAMP")
      }
      if (is.factor(vector)) {
        return("TEXT")
      }
      if (is.list(vector)) {
        return("JSONB")
      }
      return("TEXT")
    }
  )
)
