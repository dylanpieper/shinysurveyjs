#' Read and Cache Tables
#'
#' @description
#' Creates a cache of database tables for efficient access.
#' Tables are read from the database and stored in a list.
#'
#' @param db_ops Database operations object that contains methods for reading tables.
#'   Must have a `read_table` method that accepts a table name as parameter.
#' @param dynamic_config List of table configurations. Each configuration must be a list
#'   containing at least a `table_name` field specifying which table to read.
#'
#' @return A named list of cached tables.
#'   Access individual tables using `tables$table_name`.
#'
#' @examples
#' \dontrun{
#' # In server.R
#' config <- list(
#'   list(
#'     table_name = "config_packages",
#'     group_type = "choice",
#'     group_col = "package",
#'     choices_col = "version"
#'   ),
#'   list(
#'     table_name = "config_pid",
#'     group_type = "param",
#'     group_col = "pid",
#'     display_col = "full_name"
#'   )
#' )
#' tables <- read_and_cache(db_ops, config)
#'
#' # Access cached table
#' output$packageTable <- renderTable({
#'   tables$config_packages
#' })
#' }
#'
#' @export
read_and_cache <- function(db_ops, dynamic_config) {
  # Initialize empty list to store tables
  tables_cache <- list()

  # Read and store each table
  for (config in dynamic_config) {
    table_name <- config$table_name
    tables_cache[[table_name]] <- db_ops$read_table(table_name)
  }

  # Return the cache
  return(tables_cache)
}

#' Validate Dynamic Configuration
#'
#' @description
#' Validates the structure and content of the dynamic configuration list.
#' Checks that all required fields are present and valid for each configuration entry.
#' Logs validation results using the provided survey logger.
#'
#' @param dynamic_config List of configuration entries. Each entry must be a list
#'   containing the following fields:
#'   - table_name: Name of the database table
#'   - group_type: Must be either "choice" or "param"
#'   - group_col: Column name for grouping
#'   - choices_col: (Required for group_type="choice") Column name for choices
#'   - display_col: (Required for group_type="param") Column name for display
#'
#' @param config_list Optional list of cached tables to verify table/column existence
#' @param survey_logger `survey_logger` object for recording validation results and errors
#'
#' @return List with two elements:
#'   - valid: Logical indicating if the configuration is valid
#'   - errors: Character vector of error messages (empty if valid)
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   list(
#'     table_name = "config_packages",
#'     group_type = "choice",
#'     group_col = "package",
#'     choices_col = "version"
#'   ),
#'   list(
#'     table_name = "config_pid",
#'     group_type = "param",
#'     group_col = "pid",
#'     display_col = "full_name"
#'   )
#' )
#'
#' # First cache the tables
#' config_list <- read_and_cache(db_ops, config)
#'
#' # Then validate using the cache
#' result <- dynamic_config_validate(config, config_list, survey_logger)
#' if (!result$valid) {
#'   stop(paste(result$errors, collapse = "\n"))
#' }
#' }
#'
#' @export
dynamic_config_validate <- function(dynamic_config, config_list = NULL, survey_logger = NULL) {
  errors <- character()

  # Log validation start
  if (!is.null(survey_logger)) {
    survey_logger$log_message("Started dynamic field configuration validation", zone = "SURVEY")
  }

  # Check if dynamic_config is a list
  if (!is.list(dynamic_config)) {
    error_msg <- "dynamic_config must be a list"
    if (!is.null(survey_logger)) {
      survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
    }
    return(list(
      valid = FALSE,
      errors = error_msg
    ))
  }

  # Check each configuration entry
  for (i in seq_along(dynamic_config)) {
    config <- dynamic_config[[i]]
    prefix <- sprintf("Configuration entry %d: ", i)

    # Log current entry validation
    if (!is.null(survey_logger)) {
      survey_logger$log_message(
        sprintf("Checking configuration entry %d: '%s'", i, config$table_name),
        zone = "SURVEY"
      )
    }

    # Check if config is a list
    if (!is.list(config)) {
      error_msg <- paste0(prefix, "must be a list")
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
      next
    }

    # Check required fields
    required_fields <- c("table_name", "group_type", "group_col")
    missing_fields <- required_fields[!required_fields %in% names(config)]
    if (length(missing_fields) > 0) {
      error_msg <- paste0(prefix, "missing required fields: ",
                          paste(missing_fields, collapse = ", "))
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
    }

    # Check group_type validity
    if ("group_type" %in% names(config)) {
      if (!config$group_type %in% c("choice", "param")) {
        error_msg <- paste0(prefix, "group_type must be either 'choice' or 'param'")
        errors <- c(errors, error_msg)
        if (!is.null(survey_logger)) {
          survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
        }
      } else {
        if (config$group_type == "param" && !"display_col" %in% names(config)) {
          error_msg <- paste0(prefix, "display_col is required for group_type='param'")
          errors <- c(errors, error_msg)
          if (!is.null(survey_logger)) {
            survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
          }
        }
      }
    }

    # If config_list is provided, verify table and column existence
    if (!is.null(config_list) && "table_name" %in% names(config)) {
      # Check if table exists in cache
      if (!config$table_name %in% names(config_list)) {
        error_msg <- paste0(prefix, "table '", config$table_name, "' not found in cache")
        errors <- c(errors, error_msg)
        if (!is.null(survey_logger)) {
          survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
        }
      } else {
        table_data <- config_list[[config$table_name]]

        # Check if columns exist
        cols_to_check <- c(config$group_col)
        if ("choices_col" %in% names(config)) cols_to_check <- c(cols_to_check, config$choices_col)
        if ("display_col" %in% names(config)) cols_to_check <- c(cols_to_check, config$display_col)

        missing_cols <- cols_to_check[!cols_to_check %in% names(table_data)]
        if (length(missing_cols) > 0) {
          error_msg <- paste0(prefix, "columns not found in table: ",
                              paste(missing_cols, collapse = ", "))
          errors <- c(errors, error_msg)
          if (!is.null(survey_logger)) {
            survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
          }
        }
      }
    }
  }

  # Log final validation result
  if (!is.null(survey_logger)) {
    if (length(errors) == 0) {
      survey_logger$log_message("Validated dynamic field configuration", zone = "SURVEY")
    } else {
      survey_logger$log_message(
        sprintf("Dynamic field configuration validation failed with %d errors", length(errors)),
        type = "ERROR",
        zone = "SURVEY"
      )
    }
  }

  # Return validation results
  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

#' Validate URL Parameters Against Config Tables
#'
#' @description
#' Validates URL parameters against the cached configuration tables for entries
#' with group_type = "param". Checks if the parameter values exist in the
#' corresponding tables.
#'
#' @param dynamic_config List of configuration entries
#' @param config_list Cached tables from read_and_cache
#' @param query_list List of URL parameters from parse_query
#' @param survey_logger Logger object for recording validation results
#'
#' @return List with validation results:
#'   - valid: Logical indicating if all parameters are valid
#'   - errors: Character vector of error messages
#'   - values: List of validated parameter values
#'
#' @export
url_parameters_validate <- function(dynamic_config, config_list, query_list, survey_logger = NULL) {
  # Initialize results
  errors <- character()
  validated_values <- list()

  # Log validation start
  if (!is.null(survey_logger)) {
    survey_logger$log_message("Starting URL parameter validation", zone = "SURVEY")
  }

  # Loop through config entries with group_type = "param"
  param_configs <- Filter(function(x) x$group_type == "param", dynamic_config)

  for (config in param_configs) {
    param_name <- config$group_col
    table_name <- config$table_name

    # Log current parameter check
    if (!is.null(survey_logger)) {
      survey_logger$log_message(
        sprintf("Checking parameter '%s' in table '%s'", param_name, table_name),
        zone = "SURVEY"
      )
    }

    # Check if parameter exists in URL
    param_value <- query_list[[param_name]]
    if (is.null(param_value)) {
      error_msg <- sprintf("Required parameter '%s' not found in URL", param_name)
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
      next
    }

    # Get cached table data
    table_data <- config_list[[table_name]]
    if (is.null(table_data)) {
      error_msg <- sprintf("Cache missing for table '%s'", table_name)
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
      next
    }

    # Check if value exists in table
    if (!param_value %in% table_data[[param_name]]) {
      error_msg <- sprintf(
        "Invalid value '%s' for parameter '%s'. Not found in table '%s'",
        param_value, param_name, table_name
      )
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
      next
    }

    # Store validated value
    validated_values[[param_name]] <- param_value

    # Log successful validation
    if (!is.null(survey_logger)) {
      survey_logger$log_message(
        sprintf("Validated parameter '%s' with value '%s'",
                param_name, param_value),
        zone = "SURVEY"
      )
    }
  }

  # Log final validation result
  if (!is.null(survey_logger)) {
    if (length(errors) != 0) {
      survey_logger$log_message(
        sprintf("URL parameter validation failed with %d errors", length(errors)),
        type = "ERROR",
        zone = "SURVEY"
      )
    }
  }

  # Return validation results
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    values = validated_values
  ))
}

#' Get Display Text for Source Parameters
#'
#' Looks up the display text from the config source table based on the source value.
#' If no match is found, returns the original value.
#'
#' @param source_value Character. The source identifier (e.g., "GITHUB", "CRAN")
#' @param config_source_df Data frame. Configuration table containing source mappings
#'   with columns 'source' and 'display_text'
#'
#' @return Character. The display text corresponding to the source value
get_source_display_text <- function(source_value, config_source_df) {
  display_text <- config_source_df$display_text[config_source_df$source == source_value]
  if(length(display_text) == 0) return(source_value)  # fallback to value if not found
  return(display_text)
}

#' Transform Validated Parameters
#'
#' Transforms a list of validated parameters into a structured format with text/value pairs,
#' looking up display text from configuration tables where applicable.
#'
#' @param validated_params List. Input parameters in the format:
#'   list(param_name = "value") or list(param_name = list(value = "value"))
#' @param config_list List. Configuration data containing:
#'   - config_source: Data frame with columns 'source' and 'display_text'
#'   - config_packages: Data frame with package configurations
#'
#' @return List. Transformed parameters in the format:
#'   list(param_name = list(text = "display_text", value = "value"))
transform_validated_params <- function(validated_params, config_list) {
  lapply(names(validated_params), function(param_name) {
    # Handle both atomic vectors and list values
    param <- validated_params[[param_name]]
    param_value <- if(is.list(param)) param$value else param

    # If it's a source parameter, look up the display text
    if(param_name == "source") {
      list(
        text = get_source_display_text(param_value, config_list$config_source),
        value = param_value
      )
    } else {
      # For other parameters, use value as both text and value
      list(
        text = param_value,
        value = param_value
      )
    }
  }) |> setNames(names(validated_params))
}
