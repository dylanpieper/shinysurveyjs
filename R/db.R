#' Open Database Connection Pool
#'
#' Creates a database connection pool using DBI-compatible database drivers.
#' Stores the pool globally and registers cleanup handler.
#'
#' @param driver DBI-compatible driver function or name of database driver (default: RMariaDB::MariaDB())
#' @param host Database host
#' @param port Database port (default: 3306)
#' @param db_name Database name
#' @param user Database username
#' @param password Database password
#' @param global Logical; if TRUE (default), assigns pool to .GlobalEnv as app_pool
#' @param logger Logger object for tracking operations (default: NULL)
#' @param pool_size Maximum number of connections in the pool (default: 10)
#' @param ... Additional connection parameters passed to driver
#'
#' @return A database connection pool object
#'
#' @importFrom pool dbPool poolClose
#' @importFrom shiny onStop
#'
#' @noRd
#' @keywords internal
db_conn_open <- function(
    driver = RMariaDB::MariaDB(),
    host = NULL,
    port = 3306,
    db_name = NULL,
    user = NULL,
    password = NULL,
    global = TRUE,
    logger = NULL,
    pool_size = 10,
    ...) {
  if (!is.null(driver) && !is.null(host) && !is.null(port) && !is.null(db_name) && !is.null(user) && !is.null(password)) {
    
    # Create connection pool
    pool <- pool::dbPool(
      drv = driver,
      host = host,
      port = port,
      dbname = db_name,
      user = user,
      password = password,
      maxSize = pool_size,
      ...
    )

    if (global) {
      assign("app_pool", pool, envir = .GlobalEnv)

      shiny::onStop(function() {
        db_conn_close(NULL, logger)
      })

      if (!is.null(logger)) {
        logger$log_message(paste("Database connection pool initialized for", class(driver)[1], "with", pool_size, "max connections"), "INFO", "DATABASE")
      }
    }

    return(pool)
  } else {
    cli::cli_abort("Database connection parameters are required")
  }
}

#' Close Global Database Connection Pool
#'
#' Closes the global database connection pool (app_pool) during application shutdown.
#'
#' @param session Shiny session object (unused)
#' @param logger Logger object for tracking cleanup operations.
#'               If NULL, no logging is performed.
#'
#' @return Invisible NULL
#'
#' @importFrom pool poolClose
#' @noRd
#' @keywords internal
db_conn_close <- function(session, logger = NULL) {
  if (exists("app_pool", envir = .GlobalEnv)) {
    pool <- get("app_pool", envir = .GlobalEnv)
    pool::poolClose(pool)
    rm("app_pool", envir = .GlobalEnv)
    if (!is.null(logger)) {
      logger$log_message("Closed connection pool and removed object", "INFO", "DATABASE")
    }
  }
  invisible(NULL)
}

#' Database Operations Class
#'
#' @description
#' R6 Class for managing database operations related to survey data storage
#' and retrieval using MySQL and other DBI-compatible databases.
#'
#' @details
#' This class handles all database interactions for survey data, including:
#' * Table creation and modification
#' * Data insertion and retrieval
#' * Transaction management
#' * Error handling and logging
#'
#' @import R6
#' @importFrom DBI dbExecute dbQuoteIdentifier dbGetQuery dbBegin dbCommit
#' @importFrom DBI dbRollback dbIsValid dbExistsTable dbWriteTable
#' @importFrom shiny getDefaultReactiveDomain parseQueryString
db_ops <- R6::R6Class(
  "Database Operations",
  public = list(

    #' @field conn Database connection
    conn = NULL,

    #' @field logger Logger instance for tracking operations
    logger = NULL,

    #' @description Create a new Database Operations instance
    #' @param conn Connection pool object. Database connection pool
    #' @param logger survey_logger object. Logger instance for tracking operations
    initialize = function(conn, logger) {
      if (is.null(conn)) {
        logger$log_message("Database connection pool cannot be NULL", "ERROR", "DATABASE")
        stop("Database connection pool is required")
      }
      self$conn <- conn
      self$logger <- logger
    },

    #' @description Execute a database operation with transaction handling using connection pool
    #' @param operation Function. The database operation to execute
    #' @param error_message Character. Message to display if operation fails
    #' @return Result of the operation or error message if failed
    operate = function(operation, error_message) {
      if (is.null(self$conn)) {
        self$logger$log_message("Database connection pool is not valid", "ERROR", "DATABASE")
        stop("Database connection pool not valid")
      }

      # Get a connection from the pool for this transaction
      conn <- pool::poolCheckout(self$conn)
      on.exit(pool::poolReturn(conn))

      tryCatch(
        {
          DBI::dbBegin(conn)
          result <- operation(conn)
          DBI::dbCommit(conn)

          return(result)
        },
        error = function(e) {
          if (DBI::dbIsValid(conn)) {
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
          # Log database errors with SQL statement if available
          # Force log for critical operations (connection, table creation)
          force_critical <- grepl("connection|initialize|create", error_message, ignore.case = TRUE)
          self$logger$log_entry(
            survey_id = NA, # Will be set by calling function if available
            message = sprintf("%s: %s", error_message, e$message),
            sql_statement = self$logger$last_sql_statement,
            force_log = force_critical
          )
          stop(sprintf("%s: %s", error_message, e$message))
        }
      )
    },


    #' @description Check pre-defined survey table for required columns
    #' @param write_table Character. Name of the pre-defined table to check
    #' @param data data.frame. Data frame containing the required columns for the survey
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
          self$logger$log_message(
            sprintf("Pre-defined table '%s' does not exist", table_name),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf("Pre-defined table '%s' not found. Please ensure the table exists before running the survey.", table_name))
        }

        # Get existing columns from the pre-defined table
        cols_query <- sprintf(
          "SELECT COLUMN_NAME as column_name FROM information_schema.columns WHERE table_name = '%s' AND table_schema = DATABASE();",
          table_name
        )
        self$logger$update_last_sql_statement(cols_query)
        existing_cols <- DBI::dbGetQuery(conn, cols_query)$column_name

        # Check for required columns
        missing_cols <- character(0)
        for (col in names(data)) {
          if (!col %in% existing_cols) {
            # Check if this field has showOtherItem enabled and needs an _other column
            if (!is.null(survey_obj) && private$has_other_option(survey_obj, col)) {
              other_col_name <- paste0(col, "_other")
              if (!other_col_name %in% existing_cols) {
                missing_cols <- c(missing_cols, col, other_col_name)
              } else if (!col %in% existing_cols) {
                missing_cols <- c(missing_cols, col)
              }
            } else {
              missing_cols <- c(missing_cols, col)
            }
          }
        }

        if (length(missing_cols) > 0) {
          self$logger$log_message(
            sprintf(
              "Pre-defined table '%s' is missing required columns: %s",
              table_name, paste(missing_cols, collapse = ", ")
            ),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf(
            "Pre-defined table '%s' is missing required columns: %s. Please add these columns to the table.",
            table_name, paste(missing_cols, collapse = ", ")
          ))
        }

        self$logger$log_message(
          sprintf("Validated pre-defined table '%s' has all required columns", table_name),
          "INFO",
          "DATABASE"
        )
      }, "Failed to validate survey table")

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
            sprintf("Pre-defined table '%s' does not exist", table_name),
            "ERROR",
            "table_check"
          )
          stop(sprintf("Pre-defined table '%s' does not exist", table_name))
        }

        # Get existing columns from the pre-defined table
        cols_query <- sprintf(
          "SELECT COLUMN_NAME as column_name, DATA_TYPE as data_type
           FROM information_schema.columns
           WHERE table_name = '%s' AND table_schema = DATABASE();",
          table_name
        )
        self$logger$update_last_sql_statement(cols_query)
        existing_cols <- DBI::dbGetQuery(conn, cols_query)

        # Check that all required columns exist
        missing_cols <- setdiff(names(data), existing_cols$column_name)
        if (length(missing_cols) > 0) {
          self$logger$log_message(
            sprintf(
              "Pre-defined table '%s' is missing required columns: %s",
              table_name, paste(missing_cols, collapse = ", ")
            ),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf(
            "Pre-defined table '%s' is missing required columns: %s",
            table_name, paste(missing_cols, collapse = ", ")
          ))
        }

        # Filter data to only include columns that exist in the table
        available_data_cols <- intersect(names(data), existing_cols$column_name)
        if (length(available_data_cols) == 0) {
          self$logger$log_message(
            sprintf("No matching columns found between data and table '%s'", table_name),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf("No matching columns found between data and table '%s'", table_name))
        }

        # Select only the columns that exist in the table
        filtered_data <- data[, available_data_cols, drop = FALSE]

        # Insert data - construct actual INSERT statement for logging
        columns <- paste(DBI::dbQuoteIdentifier(conn, names(filtered_data)), collapse = ", ")
        values_list <- apply(filtered_data, 1, function(row) {
          values <- vapply(row, function(val) {
            if (is.na(val) || is.null(val)) {
              "NULL"
            } else if (is.character(val)) {
              DBI::dbQuoteString(conn, val)
            } else {
              as.character(val)
            }
          }, character(1))
          paste("(", paste(values, collapse = ", "), ")")
        })

        insert_statement <- sprintf(
          "INSERT INTO %s (%s) VALUES %s",
          DBI::dbQuoteIdentifier(conn, table_name),
          columns,
          paste(values_list, collapse = ", ")
        )
        self$logger$update_last_sql_statement(insert_statement)

        DBI::dbWriteTable(
          conn,
          name = table_name,
          value = filtered_data,
          append = TRUE,
          row.names = FALSE
        )

        self$logger$log_message(
          sprintf(
            "Inserted %d rows into pre-defined table '%s' using %d columns",
            nrow(filtered_data), table_name, length(available_data_cols)
          ),
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
    #' @param update_last_sql Logical. If TRUE, update logger's last_sql_statement (default: TRUE)
    #' @return data.frame. The requested data
    read_table = function(table_name, columns = NULL, filters = NULL,
                          order_by = NULL, desc = FALSE, limit = NULL, update_last_sql = TRUE) {
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

        if (update_last_sql) {
          self$logger$update_last_sql_statement(query)
        }
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

        self$logger$update_last_sql_statement(query)
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

    #' @description Perform survey update operation using join columns
    #' @param source_data data.frame. Data from the source survey to use for updates
    #' @param target_table Character. Name of the target table to update
    #' @param join_columns Named character vector. Columns to join on (source_col = target_col)
    #' @return Invisible(NULL)
    perform_survey_update = function(source_data, target_table, join_columns) {
      if (!is.data.frame(source_data) || nrow(source_data) == 0) {
        self$logger$log_message("Invalid source_data: must be a non-empty data frame", "ERROR", "DATABASE")
        stop("Invalid source data format")
      }

      if (is.null(target_table) || !is.character(target_table)) {
        self$logger$log_message("Invalid target_table parameter", "ERROR", "DATABASE")
        stop("Invalid target table name")
      }

      if (is.null(join_columns) || !is.character(join_columns) || length(join_columns) == 0) {
        self$logger$log_message("Invalid join_columns: must be a named character vector", "ERROR", "DATABASE")
        stop("Invalid join columns")
      }

      sanitized_target <- private$sanitize_survey_table_name(target_table)

      self$operate(function(conn) {
        if (!DBI::dbExistsTable(conn, sanitized_target)) {
          self$logger$log_message(
            sprintf("Target table '%s' does not exist", sanitized_target),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf("Target table '%s' does not exist", sanitized_target))
        }

        # Extract join values from source data
        source_col <- names(join_columns)[1] # e.g., "grant_drops_id"
        target_col <- join_columns[1] # e.g., "id"

        if (!source_col %in% names(source_data)) {
          self$logger$log_message(
            sprintf("Join column '%s' not found in source data", source_col),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf("Join column '%s' not found in source data", source_col))
        }

        join_value <- source_data[[source_col]]
        if (is.na(join_value) || is.null(join_value)) {
          self$logger$log_message(
            sprintf("Join value for column '%s' is NA or NULL", source_col),
            "ERROR",
            "DATABASE"
          )
          stop(sprintf("Join value for column '%s' is NA or NULL", source_col))
        }

        # Prepare update data (exclude the join column from updates)
        update_data <- source_data[!names(source_data) %in% source_col]

        if (ncol(update_data) == 0) {
          self$logger$log_message("No columns to update after excluding join column", "WARNING", "DATABASE")
          return(invisible(NULL))
        }

        # Check if target record exists
        check_query <- sprintf(
          "SELECT COUNT(*) as count FROM %s WHERE %s = %s",
          DBI::dbQuoteIdentifier(conn, sanitized_target),
          DBI::dbQuoteIdentifier(conn, target_col),
          if (is.character(join_value)) DBI::dbQuoteString(conn, join_value) else join_value
        )
        self$logger$update_last_sql_statement(check_query)
        record_count <- DBI::dbGetQuery(conn, check_query)$count

        if (record_count == 0) {
          self$logger$log_message(
            sprintf("No record found in '%s' with %s = %s", sanitized_target, target_col, join_value),
            "WARNING",
            "DATABASE"
          )
          return(invisible(NULL))
        }

        # Build SET clause for UPDATE
        set_parts <- vapply(names(update_data), function(col) {
          value <- update_data[[col]]
          if (is.na(value) || is.null(value)) {
            sprintf("%s = NULL", DBI::dbQuoteIdentifier(conn, col))
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

        set_clause <- paste(set_parts, collapse = ", ")

        # Build and execute UPDATE query
        update_query <- sprintf(
          "UPDATE %s SET %s WHERE %s = %s",
          DBI::dbQuoteIdentifier(conn, sanitized_target),
          set_clause,
          DBI::dbQuoteIdentifier(conn, target_col),
          if (is.character(join_value)) DBI::dbQuoteString(conn, join_value) else join_value
        )

        self$logger$update_last_sql_statement(update_query)
        rows_affected <- DBI::dbExecute(conn, update_query)

        self$logger$log_message(
          sprintf(
            "Updated %d rows in table '%s' using join %s = %s",
            rows_affected, sanitized_target, target_col, join_value
          ),
          "INFO",
          "DATABASE"
        )

        invisible(NULL)
      }, sprintf("Failed to perform survey update on table '%s'", sanitized_target))
    },

    #' @description Get required columns for a survey to help with pre-defined table setup
    #' @param data data.frame. Data frame containing the survey data structure
    #' @param survey_obj Survey.JS definition object that contains the complete survey structure.
    #'   Used to identify fields with "other" options that need separate columns.
    #' @return List containing required column names and their recommended MySQL types
    get_required_columns = function(data, survey_obj = NULL) {
      if (!is.data.frame(data)) {
        self$logger$log_message("Invalid data: must be a data frame", "ERROR", "DATABASE")
        stop("Invalid data format")
      }

      required_cols <- list()

      for (col in names(data)) {
        # Check if this field has showOtherItem enabled
        if (!is.null(survey_obj) && private$has_other_option(survey_obj, col)) {
          # Main column for choices
          main_type <- private$get_mysql_type_for_choices(data[[col]], col, survey_obj)
          required_cols[[col]] <- main_type

          # Other column for free text responses
          other_col_name <- paste0(col, "_other")
          required_cols[[other_col_name]] <- "TEXT"
        } else {
          # Regular field
          col_type <- private$get_mysql_type(data[[col]], col, survey_obj)
          required_cols[[col]] <- col_type
        }
      }

      self$logger$log_message(
        sprintf("Generated column requirements for %d fields", length(names(data))),
        "INFO",
        "DATABASE"
      )

      return(required_cols)
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
    get_fields_with_other_option = function(survey_obj) {
      if (is.null(survey_obj)) {
        return(character(0))
      }

      fields_with_other <- character(0)

      # Helper function to recursively search elements
      search_elements <- function(elements) {
        for (element in elements) {
          if (!is.null(element$name) &&
            !is.null(element$showOtherItem) &&
            element$showOtherItem) {
            fields_with_other <<- c(fields_with_other, element$name)
          }

          # Check nested elements (e.g., in panels)
          if (!is.null(element$elements)) {
            search_elements(element$elements)
          }
        }
      }

      # Search through all pages
      if (!is.null(survey_obj$pages)) {
        for (page in survey_obj$pages) {
          if (!is.null(page$elements)) {
            search_elements(page$elements)
          }
        }
      }

      # Also search direct elements (if not using pages)
      if (!is.null(survey_obj$elements)) {
        search_elements(survey_obj$elements)
      }

      return(unique(fields_with_other))
    },
    get_mysql_type = function(vector, field_name = NULL, survey_obj = NULL) {
      # Note: Fields with showOtherItem are now handled separately in generate_column_definitions
      # This function only determines type for regular fields or when called independently

      # MySQL type detection based on data
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
    get_mysql_type_for_choices = function(vector, field_name = NULL, survey_obj = NULL) {
      # For fields with showOtherItem, determine type based on expected choice values
      # Most survey fields with choices will be numeric IDs, but check the actual data
      if (is.numeric(vector)) {
        if (all(vector == floor(vector), na.rm = TRUE)) {
          return("BIGINT")
        }
        return("DECIMAL(10,2)")
      }
      # Default to TEXT for choice fields that might contain mixed types
      return("TEXT")
    },
    generate_column_definitions = function(data, survey_obj = NULL) {
      col_defs <- c()
      created_cols <- character(0) # Track all created columns (main and _other)

      for (col in unique(names(data))) { # Use unique() to prevent processing duplicate column names
        # Skip if main column already created
        if (col %in% created_cols) {
          next
        }

        # Check if this field has showOtherItem enabled
        if (!is.null(survey_obj) && private$has_other_option(survey_obj, col)) {
          # Create main column with appropriate type for the choices (usually INT)
          main_type <- private$get_mysql_type_for_choices(data[[col]], col, survey_obj)
          col_defs <- c(col_defs, sprintf(
            "%s %s",
            DBI::dbQuoteIdentifier(self$conn, col),
            main_type
          ))
          created_cols <- c(created_cols, col)

          # Create separate column for "other" responses - check for duplicates
          other_col_name <- paste0(col, "_other")
          if (!other_col_name %in% created_cols) {
            col_defs <- c(col_defs, sprintf(
              "%s TEXT",
              DBI::dbQuoteIdentifier(self$conn, other_col_name)
            ))
            created_cols <- c(created_cols, other_col_name)

            self$logger$log_message(
              sprintf("Field '%s' has showOtherItem enabled, creating separate '_other' column", col),
              "INFO",
              "DATABASE"
            )
          }
        } else {
          # Regular field without other option
          type <- private$get_mysql_type(data[[col]], col, survey_obj)
          col_defs <- c(col_defs, sprintf(
            "%s %s",
            DBI::dbQuoteIdentifier(self$conn, col),
            type
          ))
          created_cols <- c(created_cols, col)
        }
      }

      return(col_defs)
    }
  )
)
