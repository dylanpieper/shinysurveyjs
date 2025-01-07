#' @title Open Database Pool
#'
#' @description Creates and manages a global database pool connection using PostgreSQL.
#'
#' @format An R6 class object
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
#' @description
#' R6 Class for managing database operations related to survey data storage
#' and retrieval using PostgreSQL. Includes automatic tracking of creation date,
#' update date, session ID, and IP address.
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
#' @importFrom pool poolCheckout poolReturn
#' @importFrom shiny getDefaultReactiveDomain parseQueryString
db_ops <- R6::R6Class(
  "Database Operations",

  public = list(
    #' @field session_id Unique identifier for the current session
    session_id = NULL,

    #' @field pool Database connection pool
    pool = NULL,

    #' @field logger Logger instance for tracking operations
    logger = NULL,

    #' @description Create a new Database Operations instance
    #' @param pool Pool object. Database connection pool
    #' @param session_id Character. Unique identifier for the current session
    #' @param logger survey_logger object. Logger instance for tracking operations
    initialize = function(pool, session_id, logger) {
      if (is.null(pool)) {
        logger$log_message("Database pool cannot be NULL", "ERROR", "DATABASE")
        stop("Database pool is required")
      }
      self$pool <- pool
      self$session_id <- session_id
      self$logger <- logger

      private$init_tracking_triggers()
    },

    #' @description Execute a database operation with transaction handling
    #' @param operation Function. The database operation to execute
    #' @param error_message Character. Message to display if operation fails
    #' @return Result of the operation or error message if failed
    operate = function(operation, error_message) {
      if (is.null(self$pool)) {
        self$logger$log_message("Database pool is not initialized", "ERROR", "DATABASE")
        stop("Database pool not initialized")
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

    #' @description Ensure tracking columns exist in a table
    #' @param table_name Character. Name of the table to check/modify
    #' @return Invisible NULL
    ensure_tracking_columns = function(table_name) {
      self$operate(function(conn) {
        # Get existing columns
        cols_query <- sprintf(
          "SELECT column_name, data_type
           FROM information_schema.columns
           WHERE table_name = '%s';",
          table_name
        )
        existing_cols <- DBI::dbGetQuery(conn, cols_query)

        # Define tracking columns with their types
        tracking_cols <- list(
          date_created = "TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP",
          date_updated = "TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP",
          session_id = "TEXT",
          ip_address = "INET"
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
                "UPDATE %s SET %s = CURRENT_TIMESTAMP WHERE %s IS NULL;",
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

        # Check if trigger exists
        trigger_name <- paste0("update_date_updated_", table_name)
        trigger_exists_query <- sprintf(
          "SELECT 1 FROM pg_trigger WHERE tgname = '%s'",
          trigger_name
        )
        trigger_exists <- nrow(DBI::dbGetQuery(conn, trigger_exists_query)) > 0

        # Create trigger if it doesn't exist
        if (!trigger_exists) {
          trigger_query <- sprintf(
            "CREATE TRIGGER %s
             BEFORE UPDATE ON %s
             FOR EACH ROW
             EXECUTE FUNCTION update_date_updated();",
            trigger_name,
            DBI::dbQuoteIdentifier(conn, table_name)
          )
          DBI::dbExecute(conn, trigger_query)

          self$logger$log_message(
            sprintf("Added update trigger to table '%s'", table_name),
            "INFO",
            "DATABASE"
          )
        }

        invisible(NULL)
      }, sprintf("Failed to ensure tracking columns for table '%s'", table_name))
    },

    #' @description Create new survey data table with tracking columns
    #' @param write_table Character. Name of the table to create
    #' @param data data.frame. Data frame containing the schema for the new table
    #' @return Character. The sanitized table name
    create_survey_table = function(write_table, data) {
      if (!is.data.frame(data) || nrow(data) == 0) {
        self$logger$log_message("Invalid data: must be a non-empty data frame", "ERROR", "DATABASE")
        stop("Invalid data format")
      }

      table_name <- private$sanitize_survey_table_name(write_table)

      self$operate(function(conn) {
        # Check if table exists
        table_exists <- DBI::dbExistsTable(conn, table_name)

        if (!table_exists) {
          # Define tracking columns
          tracking_cols <- c(
            "date_created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP",
            "date_updated TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP",
            "session_id TEXT",
            "ip_address INET"
          )

          col_defs <- private$generate_column_definitions(data)
          create_query <- sprintf(
            "CREATE TABLE %s (
              id SERIAL PRIMARY KEY,
              %s,
              %s
            );",
            DBI::dbQuoteIdentifier(conn, table_name),
            paste(col_defs, collapse = ", "),
            paste(tracking_cols, collapse = ", ")
          )
          DBI::dbExecute(conn, create_query)

          # Create trigger for automatic date_updated
          trigger_query <- sprintf(
            "CREATE TRIGGER update_date_updated_%s
             BEFORE UPDATE ON %s
             FOR EACH ROW
             EXECUTE FUNCTION update_date_updated();",
            table_name,
            DBI::dbQuoteIdentifier(conn, table_name)
          )
          DBI::dbExecute(conn, trigger_query)

          self$logger$log_message(
            sprintf("Created survey table '%s'", table_name),
            "INFO",
            "DATABASE"
          )
        } else {
          # Ensure tracking columns exist in existing table
          self$ensure_tracking_columns(table_name)

          # Add any missing columns from the data
          cols_query <- sprintf(
            "SELECT column_name FROM information_schema.columns WHERE table_name = '%s';",
            table_name
          )
          existing_cols <- DBI::dbGetQuery(conn, cols_query)$column_name

          for (col in names(data)) {
            if (!col %in% existing_cols) {
              col_type <- private$get_postgres_type(data[[col]])
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
        data$ip_address <- private$get_client_ip()

        # Check for new columns
        cols_query <- sprintf(
          "SELECT column_name, data_type
           FROM information_schema.columns
           WHERE table_name = '%s';",
          table_name
        )
        existing_cols <- DBI::dbGetQuery(conn, cols_query)

        new_cols <- setdiff(names(data), existing_cols$column_name)

        # Add new columns if needed
        for (col in new_cols) {
          if (!col %in% c("date_created", "date_updated", "session_id", "ip_address")) {
            col_type <- private$get_postgres_type(data[[col]])
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
          sprintf("Inserted survey data (n = %d)", nrow(data)),
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
        self$logger$log_message("Invalid table_name parameter", "ERROR", "DATABASE")
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
              sprintf("%s = %s",
                      DBI::dbQuoteIdentifier(conn, col),
                      DBI::dbQuoteString(conn, value))
            } else {
              sprintf("%s = %s",
                      DBI::dbQuoteIdentifier(conn, col),
                      value)
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
          sprintf("Read n = %d rows from '%s'", nrow(result), sanitized_table),
          "INFO",
          "DATABASE"
        )

        return(result)
      }, sprintf("Failed to read from table '%s'", sanitized_table))
    }
  ),

  private = list(
    init_tracking_triggers = function() {
      self$operate(function(conn) {
        # Create trigger function for updating date_updated
        trigger_func_query <- "
          CREATE OR REPLACE FUNCTION update_date_updated()
          RETURNS TRIGGER AS $$
          BEGIN
            NEW.date_updated = CURRENT_TIMESTAMP;
            RETURN NEW;
          END;
          $$ LANGUAGE plpgsql;"
        DBI::dbExecute(conn, trigger_func_query)
      }, "Failed to initialize tracking triggers")
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
        return("TIMESTAMP WITH TIME ZONE")
      }
      if (is.factor(vector)) {
        return("TEXT")
      }
      if (is.list(vector)) {
        return("JSONB")
      }
      return("TEXT")
    },

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
  )
)
