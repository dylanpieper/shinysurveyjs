#' Open Database Connection
#'
#' Creates a database connection using MySQL or other DBI-compatible drivers.
#' Stores the connection globally and registers cleanup handler.
#'
#' @param driver Character string specifying the database driver ("mysql", "sqlite", "postgres")
#' @param host Database host
#' @param port Database port
#' @param db_name Database name
#' @param user Database username
#' @param password Database password
#' @param global Logical; if TRUE (default), assigns connection to .GlobalEnv as app_conn
#' @param logger Logger object for tracking operations (default: NULL)
#' @param ... Additional connection parameters passed to driver
#'
#' @return A database connection object
#'
#' @importFrom DBI dbConnect
#' @importFrom shiny onStop
#'
#' @noRd
#' @keywords internal
db_conn_open <- function(driver = "mysql",
                         host = NULL,
                         port = NULL,
                         db_name = NULL,
                         user = NULL,
                         password = NULL,
                         global = TRUE,
                         logger = NULL,
                         ...) {
  if (all(!is.null(c(host, port, db_name, user, password)))) {
    # Get the appropriate driver
    drv <- switch(tolower(driver),
      "mysql" = {
        if (!requireNamespace("RMariaDB", quietly = TRUE)) {
          stop("RMariaDB package is required for MySQL connections")
        }
        RMariaDB::MariaDB()
      },
      "sqlite" = {
        if (!requireNamespace("RSQLite", quietly = TRUE)) {
          stop("RSQLite package is required for SQLite connections")
        }
        RSQLite::SQLite()
      },
      "postgres" = {
        if (!requireNamespace("RPostgres", quietly = TRUE)) {
          stop("RPostgres package is required for PostgreSQL connections")
        }
        RPostgres::Postgres()
      },
      stop("Unsupported driver: ", driver)
    )
    
    # Create connection
    conn <- DBI::dbConnect(
      drv,
      host = host,
      port = port,
      dbname = db_name,
      user = user,
      password = password,
      ...
    )

    if (global) {
      assign("app_conn", conn, envir = .GlobalEnv)
      
      shiny::onStop(function() {
        db_conn_close(NULL, logger)
      })

      if (!is.null(logger)) {
        logger$log_message(paste("Database connection initialized for", driver), "INFO", "DATABASE")
      }
    }
    
    return(conn)
  } else {
    cli::cli_abort("Database connection parameters are required")
  }
}

#' Close Global Database Connection
#'
#' Closes the global database connection (app_conn) during application shutdown.
#'
#' @param session Shiny session object (unused)
#' @param logger Logger object for tracking cleanup operations.
#'               If NULL, no logging is performed.
#'
#' @return Invisible NULL
#'
#' @importFrom DBI dbDisconnect dbIsValid
#' @noRd
#' @keywords internal
db_conn_close <- function(session, logger = NULL) {
  if (exists("app_conn", envir = .GlobalEnv)) {
    conn <- get("app_conn", envir = .GlobalEnv)
    if (DBI::dbIsValid(conn)) {
      DBI::dbDisconnect(conn)
    }
    rm("app_conn", envir = .GlobalEnv)
    if (!is.null(logger)) {
      logger$log_message("Closed connection and removed object", "INFO", "DATABASE")
    }
  }
  invisible(NULL)
}


#' Database Operations Class
#'
#' @description
#' R6 Class for managing database operations related to survey data storage
#' and retrieval using MySQL and other DBI-compatible databases. Includes automatic 
#' tracking of creation date, update date, session ID, and IP address.
#'
#' @details
#' This class handles all database interactions for survey data, including:
#' * Table creation and modification with tracking columns
#' * Data insertion with automatic timestamp management
#' * Session and IP tracking
#' * Transaction management
#' * Error handling and logging
#'
#' Tracking columns automatically added to each table:
#' * date_created: Timestamp when record was created
#' * date_updated: Timestamp when record was last updated
#' * session_id: Shiny session identifier
#' * ip_address: Client IP address
#'
#' @import R6
#' @importFrom DBI dbExecute dbQuoteIdentifier dbGetQuery dbBegin dbCommit
#' @importFrom DBI dbRollback dbIsValid dbExistsTable dbWriteTable
#' @importFrom shiny getDefaultReactiveDomain parseQueryString
db_ops <- R6::R6Class(
  "Database Operations",
  public = list(
    #' @field session_id Unique identifier for the current session
    session_id = NULL,

    #' @field conn Database connection
    conn = NULL,

    #' @field logger Logger instance for tracking operations
    logger = NULL,

    #' @description Create a new Database Operations instance
    #' @param conn Connection object. Database connection
    #' @param session_id Character. Unique identifier for the current session
    #' @param logger survey_logger object. Logger instance for tracking operations
    initialize = function(conn, session_id, logger) {
      if (is.null(conn)) {
        logger$log_message("Database connection cannot be NULL", "ERROR", "DATABASE")
        stop("Database connection is required")
      }
      self$conn <- conn
      self$session_id <- session_id
      self$logger <- logger
    },

    #' @description Execute a database operation with transaction handling
    #' @param operation Function. The database operation to execute
    #' @param error_message Character. Message to display if operation fails
    #' @return Result of the operation or error message if failed
    operate = function(operation, error_message) {
      if (is.null(self$conn) || !DBI::dbIsValid(self$conn)) {
        self$logger$log_message("Database connection is not valid", "ERROR", "DATABASE")
        stop("Database connection not valid")
      }

      tryCatch({
        DBI::dbBegin(self$conn)
        result <- operation(self$conn)
        DBI::dbCommit(self$conn)

        return(result)
      }, error = function(e) {
        if (DBI::dbIsValid(self$conn)) {
          tryCatch(
            {
              DBI::dbRollback(self$conn)
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
      })
    },

    #' @description Ensure tracking columns exist in a table
    #' @param table_name Character. Name of the table to check/modify
    #' @return Invisible NULL
    ensure_tracking_columns = function(table_name) {
      self$operate(function(conn) {
        # Get existing columns (MySQL compatible)
        cols_query <- sprintf(
          "SELECT COLUMN_NAME as column_name, DATA_TYPE as data_type
           FROM information_schema.columns
           WHERE table_name = '%s' AND table_schema = DATABASE();",
          table_name
        )
        existing_cols <- DBI::dbGetQuery(conn, cols_query)

        # Define tracking columns with their types (MySQL compatible)
        tracking_cols <- list(
          date_created = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
          date_updated = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP",
          session_id = "TEXT",
          ip_address = "VARCHAR(45)"
        )

        # Add missing tracking columns
        for (col_name in names(tracking_cols)) {
          if (!col_name %in% existing_cols$column_name) {
            alter_query <- sprintf(
              "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;",
              DBI::dbQuoteIdentifier(conn, table_name),
              DBI::dbQuoteIdentifier(conn, col_name),
              tracking_cols[[col_name]]
            )
            DBI::dbExecute(conn, alter_query)

            # For existing rows, set date_created and date_updated to current timestamp
            if (col_name %in% c("date_created", "date_updated")) {
              update_query <- sprintf(
                "UPDATE %s SET %s = NOW() WHERE %s IS NULL;",
                DBI::dbQuoteIdentifier(conn, table_name),
                DBI::dbQuoteIdentifier(conn, col_name),
                DBI::dbQuoteIdentifier(conn, col_name)
              )
              DBI::dbExecute(conn, update_query)
            }

            self$logger$log_message(
              sprintf("Added column '%s' to table '%s'", col_name, table_name),
              "INFO",
              "DATABASE"
            )
          }
        }

        # MySQL uses ON UPDATE CURRENT_TIMESTAMP in column definition instead of triggers
        # No additional trigger setup needed

        invisible(NULL)
      }, sprintf("Failed to ensure tracking columns for table '%s'", table_name))
    },

    #' @description Create new survey data table with tracking columns
    #' @param write_table Character. Name of the table to create
    #' @param data data.frame. Data frame containing the schema for the new table
    #' @param survey_obj Survey.JS definition object that contains the complete survey structure.
    #'   A nested list containing:
    #'   \describe{
    #'     \item{pages}{List of survey pages}
    #'     \item{elements}{List of survey questions/elements, where each element contains:}
    #'     \item{name}{Question identifier}
    #'     \item{type}{Question type (e.g., "checkbox", "text", "radio")}
    #'     \item{showOtherItem}{Logical. Whether the question has an "other" option}
    #'   }
    #'   Used to determine column types and handle special question features like "other" options.
    #'   Default: NULL
    #' @return Character. The sanitized table name
    create_survey_table = function(write_table, data, survey_obj = NULL) {
      if (!is.data.frame(data) || nrow(data) == 0) {
        self$logger$log_message("Invalid data: must be a non-empty data frame", "ERROR", "DATABASE")
        stop("Invalid data format")
      }

      table_name <- private$sanitize_survey_table_name(write_table)

      self$operate(function(conn) {
        # Check if table exists
        table_exists <- DBI::dbExistsTable(conn, table_name)

        if (!table_exists) {
          # Define tracking columns (MySQL compatible)
          tracking_cols <- c(
            "date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
            "date_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP",
            "session_id TEXT",
            "ip_address VARCHAR(45)"
          )

          col_defs <- private$generate_column_definitions(data, survey_obj)

          create_query <- sprintf(
            "CREATE TABLE %s (
            id INT AUTO_INCREMENT PRIMARY KEY,
            %s,
            %s
          );",
            DBI::dbQuoteIdentifier(conn, table_name),
            paste(col_defs, collapse = ", "),
            paste(tracking_cols, collapse = ", ")
          )
          DBI::dbExecute(conn, create_query)

          # MySQL uses ON UPDATE CURRENT_TIMESTAMP in column definition
          # No trigger needed

          self$logger$log_message(
            sprintf("Created survey table '%s'", table_name),
            "INFO",
            "DATABASE"
          )
        } else {
          # For existing table, ensure tracking columns
          self$ensure_tracking_columns(table_name)

          # Add any missing columns (MySQL compatible)
          cols_query <- sprintf(
            "SELECT COLUMN_NAME as column_name FROM information_schema.columns WHERE table_name = '%s' AND table_schema = DATABASE();",
            table_name
          )
          existing_cols <- DBI::dbGetQuery(conn, cols_query)$column_name

          for (col in names(data)) {
            if (!col %in% existing_cols) {
              col_type <- private$get_mysql_type(data[[col]], col, survey_obj)
              alter_query <- sprintf(
                "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;",
                DBI::dbQuoteIdentifier(conn, table_name),
                DBI::dbQuoteIdentifier(conn, col),
                col_type
              )
              DBI::dbExecute(conn, alter_query)
              self$logger$log_message(
                sprintf("Added column '%s' to table '%s'", col, table_name),
                "INFO",
                "DATABASE"
              )
            }
          }
        }
      }, "Failed to create/update survey table")

      invisible(table_name)
    },

    #' Update existing survey table with new data
    #'
    #' @param write_table Character. Name of the table to update
    #' @param data data.frame. Data frame containing the new data
    #' @return Character. The sanitized table name
    update_survey_table = function(write_table, data) {
      if (is.null(write_table) || !is.character(write_table)) {
        self$logger$log_message("Invalid write_table parameter", "ERROR", "DATABASE")
        stop("Invalid table name")
      }

      if (!is.data.frame(data) || nrow(data) == 0) {
        self$logger$log_message("Invalid data: must be a non-empty data frame", "ERROR", "DATABASE")
        stop("Invalid data format")
      }

      table_name <- private$sanitize_survey_table_name(write_table)

      self$operate(function(conn) {
        if (!DBI::dbExistsTable(conn, table_name)) {
          self$logger$log_message(
            sprintf("Table '%s' does not exist", table_name),
            "ERROR",
            "table_check"
          )
          stop(sprintf("Table '%s' does not exist", table_name))
        }

        # Ensure tracking columns exist
        self$ensure_tracking_columns(table_name)

        # Add tracking data
        data$session_id <- self$session_id
        data$ip_address <- self$get_client_ip()

        # Check for new columns (MySQL compatible)
        cols_query <- sprintf(
          "SELECT COLUMN_NAME as column_name, DATA_TYPE as data_type
           FROM information_schema.columns
           WHERE table_name = '%s' AND table_schema = DATABASE();",
          table_name
        )
        existing_cols <- DBI::dbGetQuery(conn, cols_query)

        new_cols <- setdiff(names(data), existing_cols$column_name)

        # Add new columns if needed
        for (col in new_cols) {
          if (!col %in% c("date_created", "date_updated", "session_id", "ip_address")) {
            col_type <- private$get_mysql_type(data[[col]])
            alter_query <- sprintf(
              "ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;",
              DBI::dbQuoteIdentifier(conn, table_name),
              DBI::dbQuoteIdentifier(conn, col),
              col_type
            )
            DBI::dbExecute(conn, alter_query)
            self$logger$log_message(
              sprintf("Added column '%s' to '%s'", col, table_name),
              "INFO",
              "DATABASE"
            )
          }
        }

        # Insert data
        DBI::dbWriteTable(
          conn,
          name = table_name,
          value = data,
          append = TRUE,
          row.names = FALSE
        )

        self$logger$log_message(
          sprintf("Inserted %d rows into '%s'", nrow(data), table_name),
          "INFO",
          "DATABASE"
        )
      }, sprintf("Failed to update table '%s'", table_name))

      invisible(table_name)
    },

    #' @description Read data from a survey table with optional filtering
    #' @param table_name Character. Name of the table to read from
    #' @param columns Character vector. Specific columns to read (NULL for all columns)
    #' @param filters List. Named list of filter conditions (e.g., list(status = "active"))
    #' @param order_by Character vector. Columns to order by
    #' @param desc Logical. If TRUE, sort in descending order
    #' @param limit Numeric. Maximum number of rows to return (NULL for all rows)
    #' @return data.frame. The requested data
    read_table = function(table_name, columns = NULL, filters = NULL,
                          order_by = NULL, desc = FALSE, limit = NULL) {
      if (is.null(table_name) || !is.character(table_name)) {
        self$logger$log_message(paste("Invalid table_name parameter:", table_name), "ERROR", "DATABASE")
        stop("Invalid table name")
      }

      sanitized_table <- private$sanitize_survey_table_name(table_name)

      self$operate(function(conn) {
        # Check if table exists
        if (!DBI::dbExistsTable(conn, sanitized_table)) {
          self$logger$log_message(
            sprintf("Table '%s' does not exist", sanitized_table),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf("Table '%s' does not exist", sanitized_table))
        }

        # Build SELECT clause
        select_cols <- "*"
        if (!is.null(columns)) {
          if (!is.character(columns)) {
            self$logger$log_message("Columns must be a character vector", "ERROR", "DATABASE")
            stop("Invalid columns parameter")
          }
          select_cols <- paste(
            vapply(columns, function(col) {
              DBI::dbQuoteIdentifier(conn, col)
            }, character(1)),
            collapse = ", "
          )
        }

        # Build WHERE clause
        where_clause <- ""
        if (!is.null(filters)) {
          if (!is.list(filters)) {
            self$logger$log_message("Filters must be a named list", "ERROR", "DATABASE")
            stop("Invalid filters parameter")
          }

          where_conditions <- vapply(names(filters), function(col) {
            value <- filters[[col]]
            if (is.null(value)) {
              sprintf("%s IS NULL", DBI::dbQuoteIdentifier(conn, col))
            } else if (is.character(value)) {
              sprintf(
                "%s = %s",
                DBI::dbQuoteIdentifier(conn, col),
                DBI::dbQuoteString(conn, value)
              )
            } else {
              sprintf(
                "%s = %s",
                DBI::dbQuoteIdentifier(conn, col),
                value
              )
            }
          }, character(1))

          if (length(where_conditions) > 0) {
            where_clause <- paste("WHERE", paste(where_conditions, collapse = " AND "))
          }
        }

        # Build ORDER BY clause
        order_clause <- ""
        if (!is.null(order_by)) {
          if (!is.character(order_by)) {
            self$logger$log_message("order_by must be a character vector", "ERROR", "DATABASE")
            stop("Invalid order_by parameter")
          }
          direction <- if (desc) "DESC" else "ASC"
          order_cols <- paste(
            vapply(order_by, function(col) {
              paste(DBI::dbQuoteIdentifier(conn, col), direction)
            }, character(1)),
            collapse = ", "
          )
          order_clause <- paste("ORDER BY", order_cols)
        }

        # Build LIMIT clause
        limit_clause <- ""
        if (!is.null(limit)) {
          if (!is.numeric(limit) || limit < 0) {
            self$logger$log_message("limit must be a non-negative number", "ERROR", "DATABASE")
            stop("Invalid limit parameter")
          }
          limit_clause <- sprintf("LIMIT %d", as.integer(limit))
        }

        # Construct and execute query
        query <- sprintf(
          "SELECT %s FROM %s %s %s %s",
          select_cols,
          DBI::dbQuoteIdentifier(conn, sanitized_table),
          where_clause,
          order_clause,
          limit_clause
        )

        result <- DBI::dbGetQuery(conn, query)

        self$logger$log_message(
          sprintf("Read %d rows from '%s'", nrow(result), sanitized_table),
          "INFO",
          "DATABASE"
        )

        return(result)
      }, sprintf("Failed to read from table '%s'", sanitized_table))
    },

    #' @description Update specific columns in a table for a given row ID
    #' @param table_name Character. Name of the table to update
    #' @param id Numeric. Row ID to update
    #' @param values List. Named list of column-value pairs to update
    #' @return Invisible(NULL)
    update_by_id = function(table_name, id, values) {
      if (is.null(table_name) || !is.character(table_name)) {
        self$logger$log_message("Invalid table_name parameter", "ERROR", "DATABASE")
        stop("Invalid table name")
      }

      if (is.null(id) || !is.numeric(id)) {
        self$logger$log_message("Invalid id parameter", "ERROR", "DATABASE")
        stop("Invalid id")
      }

      if (is.null(values) || !is.list(values) || length(values) == 0) {
        self$logger$log_message("Invalid values parameter", "ERROR", "DATABASE")
        stop("Invalid values")
      }

      sanitized_table <- private$sanitize_survey_table_name(table_name)

      self$operate(function(conn) {
        # Build SET clause
        set_parts <- vapply(names(values), function(col) {
          value <- values[[col]]
          if (is.character(value)) {
            sprintf(
              "%s = %s",
              DBI::dbQuoteIdentifier(conn, col),
              DBI::dbQuoteString(conn, value)
            )
          } else {
            sprintf(
              "%s = %s",
              DBI::dbQuoteIdentifier(conn, col),
              value
            )
          }
        }, character(1))

        set_clause <- paste(set_parts, collapse = ", ")

        # Build and execute UPDATE query
        query <- sprintf(
          "UPDATE %s SET %s WHERE id = %d",
          DBI::dbQuoteIdentifier(conn, sanitized_table),
          set_clause,
          id
        )

        rows_affected <- DBI::dbExecute(conn, query)

        if (rows_affected == 0) {
          warning(sprintf("No rows updated for id %d in table %s", id, sanitized_table))
        }

        self$logger$log_message(
          sprintf(
            "Updated %d rows in table '%s' for id %d: %s",
            rows_affected, sanitized_table, id,
            jsonlite::toJSON(list(values), auto_unbox = TRUE)
          ),
          "INFO",
          "DATABASE"
        )

        invisible(NULL)
      }, sprintf("Failed to update table '%s' for id %d", sanitized_table, id))
    },

    #' Get Client IP Address
    #'
    #' @description
    #' Retrieves the client IP address from HTTP request headers in order of preference.
    #' This method checks multiple headers to handle scenarios involving proxies and load balancers.
    #'
    #' @details
    #' The method checks the following headers in order:
    #' 1. X-Real-IP
    #' 2. X-Forwarded-For (takes first IP if multiple are present)
    #' 3. REMOTE_ADDR
    #'
    #' If no IP address is found in any header, returns "0.0.0.0" as a fallback.
    #'
    #' @return Character string containing the client IP address. Returns "0.0.0.0" if no IP address can be determined.
    #'
    #' @examples
    #' \dontrun{
    #' # Inside a Shiny server function
    #' server <- function(input, output, session) {
    #'   db_ops <- db_ops$new(pool, session$token, logger)
    #'   client_ip <- db_ops$get_client_ip()
    #' }
    #' }
    #'
    #' @importFrom shiny parseQueryString getDefaultReactiveDomain
    get_client_ip = function() {
      headers <- as.list(shiny::parseQueryString(shiny::getDefaultReactiveDomain()$REQUEST))

      ip <- headers$`X-Real-IP` %||%
        headers$`X-Forwarded-For` %||%
        headers$REMOTE_ADDR %||%
        "0.0.0.0"

      # If X-Forwarded-For contains multiple IPs, get the first one
      if (grepl(",", ip)) {
        ip <- trimws(strsplit(ip, ",")[[1]][1])
      }

      return(ip)
    }
  ),
  private = list(
    sanitize_survey_table_name = function(name) {
      tolower(gsub("[^[:alnum:]]", "_", name))
    },
    has_other_option = function(survey_obj, field_name) {
      if (is.null(survey_obj) || is.null(field_name)) {
        return(FALSE)
      }

      # Helper function to recursively search elements
      search_elements <- function(elements) {
        for (element in elements) {
          if (!is.null(element$name) &&
            element$name == field_name &&
            !is.null(element$showOtherItem) &&
            element$showOtherItem) {
            return(TRUE)
          }

          # Check nested elements (e.g., in panels)
          if (!is.null(element$elements)) {
            if (search_elements(element$elements)) {
              return(TRUE)
            }
          }
        }
        return(FALSE)
      }

      # Search through all pages
      if (!is.null(survey_obj$pages)) {
        for (page in survey_obj$pages) {
          if (!is.null(page$elements)) {
            if (search_elements(page$elements)) {
              return(TRUE)
            }
          }
        }
      }

      # Also search direct elements (if not using pages)
      if (!is.null(survey_obj$elements)) {
        if (search_elements(survey_obj$elements)) {
          return(TRUE)
        }
      }

      return(FALSE)
    },
    get_mysql_type = function(vector, field_name = NULL, survey_obj = NULL) {
      # First check if field has showOtherItem enabled
      if (!is.null(survey_obj) && !is.null(field_name)) {
        if (private$has_other_option(survey_obj, field_name)) {
          self$logger$log_message(
            sprintf("Field '%s' has showOtherItem enabled, using TEXT type", field_name),
            "INFO",
            "DATABASE"
          )
          return("TEXT")
        }
      }

      # MySQL type detection
      if (is.numeric(vector)) {
        if (all(vector == floor(vector), na.rm = TRUE)) {
          return("BIGINT")
        }
        return("DECIMAL(10,2)")
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
        return("JSON")
      }
      return("TEXT")
    },
    generate_column_definitions = function(data, survey_obj = NULL) {
      vapply(names(data), function(col) {
        type <- private$get_mysql_type(data[[col]], col, survey_obj)
        sprintf(
          "%s %s",
          DBI::dbQuoteIdentifier(self$conn, col),
          type
        )
      }, character(1))
    }
  )
)
