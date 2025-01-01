#' Initialize Database Pool
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
#' @export
initialize_pool <- function(host = NULL,
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

#' Clean Up Database Pool
#'
#' Closes the database pool and removes lock files.
#'
#' @param pool Database pool object to close
#' @importFrom pool dbIsValid poolClose
#' @export
cleanup_pool <- function(pool) {
  if (!is.null(pool) && pool::dbIsValid(pool)) {
    pool::poolClose(pool)
    message("Database pool connection closed")
  }

  if (file.exists("shiny_setup.lock")) {
    unlink("shiny_setup.lock")
  }
}

#' Database Operations Class
#'
#' @description
#' R6 Class for managing database operations related to survey data storage
#' and retrieval using PostgreSQL.
#'
#' @field session_id Character. Unique identifier for the current session
#' @field pool Pool object. Database connection pool
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbExecute dbQuoteIdentifier dbGetQuery dbBegin dbCommit
#' @importFrom DBI dbRollback dbIsValid dbExistsTable dbWriteTable
#' @importFrom pool poolCheckout poolReturn
#'
#' @export
db_ops <- R6::R6Class(
  "db_ops",
  public = list(
    session_id = NULL,
    pool = NULL,

    #' @description Initialize a new db_ops instance
    #' @param pool Database connection pool
    #' @param session_id Session identifier
    #' @return A new db_ops object
    initialize = function(pool, session_id) {
      if (is.null(pool)) {
        stop(private$format_message("Database pool cannot be NULL"))
      }
      self$pool <- pool
      self$session_id <- session_id
    },

    #' @description Create a new survey data table
    #' @param survey_name Character. Name of the survey
    #' @param data data.frame. Survey data to be stored
    #' @return Character. Name of the created table (invisible)
    create_survey_data_table = function(survey_name, data) {
      if (is.null(survey_name) || !is.character(survey_name)) {
        stop(private$format_message("Invalid survey_name"))
      }

      if (!is.data.frame(data) || nrow(data) == 0) {
        stop(private$format_message("Invalid data: must be a non-empty data frame"))
      }

      table_name <- private$sanitize_survey_table_name(survey_name)

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
          private$log_message(sprintf("Created new table '%s'", table_name))
        }
      }, sprintf("Failed to create table '%s'", table_name))

      invisible(table_name)
    },

    #' @description Check if a table exists in the database
    #' @param table_name Character. Name of the table to check
    #' @return Logical indicating if table exists
    check_table_exists = function(table_name) {
      if (is.null(table_name) || !is.character(table_name)) {
        stop(private$format_message("Invalid table_name"))
      }

      self$execute_db_operation(function(conn) {
        exists <- DBI::dbExistsTable(conn, table_name)
        private$log_message(sprintf("Table %s exists: %s", table_name, exists))
        exists
      }, sprintf("Error checking table %s", table_name))
    },

    #' @description Execute a database operation with appropriate connection handling
    #' @param operation Function. The database operation to execute
    #' @param error_message Character. Error message if operation fails
    #' @return Result of the database operation
    execute_db_operation = function(operation, error_message) {
      if (is.null(self$pool)) {
        stop(private$format_message("Database pool is not initialized"))
      }

      conn <- NULL
      tryCatch({
        conn <- pool::poolCheckout(self$pool)
        if (is.null(conn) || !DBI::dbIsValid(conn)) {
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
            },
            error = function(rollback_error) {
              private$log_message(sprintf("Rollback error: %s", rollback_error$message))
            }
          )
        }
        private$log_message(sprintf("%s: %s", error_message, e$message))
        stop(private$format_message(sprintf("%s: %s", error_message, e$message)))
      }, finally = {
        if (!is.null(conn)) {
          tryCatch(
            {
              pool::poolReturn(conn)
            },
            error = function(return_error) {
              private$log_message(sprintf(
                "Error returning connection to pool: %s",
                return_error$message
              ))
            }
          )
        }
      })
    },

    #' @description Update existing survey data table with new data
    #' @param survey_name Character. Name of the survey
    #' @param data data.frame. New survey data to append
    #' @return Character. Name of the updated table (invisible)
    update_survey_data_table = function(survey_name, data) {
      if (is.null(survey_name) || !is.character(survey_name)) {
        stop(private$format_message("Invalid survey_name"))
      }

      if (!is.data.frame(data) || nrow(data) == 0) {
        stop(private$format_message("Invalid data: must be a non-empty data frame"))
      }

      table_name <- private$sanitize_survey_table_name(survey_name)

      self$execute_db_operation(function(conn) {
        if (!DBI::dbExistsTable(conn, table_name)) {
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
          private$log_message(sprintf("Added column %s to '%s'", col, table_name))
        }

        DBI::dbWriteTable(
          conn,
          name = table_name,
          value = data,
          append = TRUE,
          row.names = FALSE
        )

        private$log_message(sprintf(
          "Inserted survey data into '%s' (n = %d)",
          table_name, nrow(data)
        ))
      }, sprintf("Failed to update table '%s'", table_name))

      invisible(table_name)
    }
  ),
  private = list(
    log_message = function(msg) {
      message(private$format_message(msg))
    },
    format_message = function(msg) {
      sprintf("[Session %s] %s", self$session_id, msg)
    },
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
