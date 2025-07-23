#' Null Coalescing Operator
#'
#' Returns the first non-null value from a list of values.
#'
#' @param x First value to check
#' @param y Second value to return if x is NULL
#'
#' @return The first non-null value
#'
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Read and Cache Tables for Dynamic Fields
#'
#' Creates a cache of database tables for efficient access by reading tables from
#' the database and storing them in a list. This prevents redundant database reads
#' during dynamic field population.
#'
#' @param db_ops Object. Database operations object with a `read_table` method that
#'   accepts a table name parameter.
#' @param dynamic_config List. Table configurations. Each configuration must contain:
#'   * `source_tbl`: Name of database table to read (replaces `table_name`)
#'   * Additional configuration fields are allowed but not used for caching
#'
#' @return Named list of data frames. Access tables using `tables$table_name`.
#'
#' @noRd
#' @keywords internal
read_and_cache <- function(db_ops, dynamic_config) {
  # Initialize empty list to store tables
  tables_cache <- list()

  # Read and store each table from both source_tbl and legacy table_name
  for (config in dynamic_config) {
    # Check for new source_tbl field first, fallback to legacy table_name
    table_name <- config$source_tbl %||% config$table_name
    
    if (!is.null(table_name)) {
      tables_cache[[table_name]] <- tryCatch({
        db_ops$read_table(table_name)
      }, error = function(e) {
        # Log warning but continue - table may not exist yet in multisurvey mode
        db_ops$logger$log_message(
          sprintf("Warning: Unable to read table '%s' during initialization: %s", table_name, e$message),
          "WARN",
          "SURVEY"
        )
        # Return empty data frame to prevent failures downstream
        data.frame()
      })
    }
  }

  # Return the cache
  return(tables_cache)
}

#' Validate URL Parameters Against Config Tables
#'
#' Validates URL query parameters against cached configuration tables marked as
#' "param" type. Ensures parameter values exist in the corresponding reference
#' tables.
#'
#' @param dynamic_config List. Configuration entries defining parameter validation rules.
#' @param config_list List. Cached configuration tables from read_and_cache().
#' @param query_list List. Parsed URL query parameters from parse_query().
#' @param survey_logger Logger. Object for recording validation results and errors.
#'
#' @return A list containing:
#'   * `valid`: Logical. `TRUE` if all parameters are valid.
#'   * `errors`: Character vector. Error messages for invalid parameters.
#'   * `values`: List. Validated and cleaned parameter values.
#'
#' @noRd
#' @keywords internal
validate_url_parameters <- function(dynamic_config, config_list, query_list, survey_logger = NULL) {
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
        sprintf(
          "Validated parameter '%s' with value '%s'",
          param_name, param_value
        ),
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
#' Retrieves the display text for a source identifier from a configuration table.
#' Returns the original value if no mapping exists.
#'
#' @param source_value Character. Source identifier (e.g., "GITHUB", "CRAN").
#' @param config_source_df Data frame. Configuration table with columns:
#'   * `source`: Source identifier
#'   * `display_text`: Display text for the source
#'
#' @return Character string containing the display text or original source value
#'
#' @noRd
#' @keywords internal
get_source_display_text <- function(source_value, config_source_df) {
  display_text <- config_source_df$display_text[config_source_df$source == source_value]
  if (length(display_text) == 0) {
    return(source_value)
  } # fallback to value if not found
  return(display_text)
}

#' Transform Validated Parameters
#'
#' Transforms a list of validated parameters into a structured format with text/value pairs,
#' looking up display text from configuration tables where applicable.
#'
#' @param validated_params List of parameters in format `list(param_name = "value")` or
#'   `list(param_name = list(value = "value"))`.
#' @param config_list List of configuration data frames:
#'   * `config_source`: Table with 'source' and 'display_text' columns
#'   * `config_packages`: Table with package configurations
#'
#' @return List of transformed parameters in format
#'   `list(param_name = list(text = "display_text", value = "value"))`
#'
#' @noRd
#' @keywords internal
transform_validated_params <- function(validated_params, config_list) {
  lapply(names(validated_params), function(param_name) {
    # Handle both atomic vectors and list values
    param <- validated_params[[param_name]]
    param_value <- if (is.list(param)) param$value else param

    # If it's a source parameter, look up the display text
    if (param_name == "source") {
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

#' Format Choices for JavaScript Survey Library
#'
#' Formats R choice data structures into a format compatible with JavaScript survey
#' components. Handles both flat lists of choices and hierarchical choice structures,
#' with support for parent-child relationships between fields.
#'
#' @param choices Vector or list. Either a vector of choices or a list of hierarchical
#'   choices.
#' @param is_parent Logical. Whether this field is a parent field. Default: `FALSE`.
#' @param child_field String. Name of the field containing child choices.
#' @param is_child Logical. Whether this field contains child choices. Default: `FALSE`.
#' @param parent_field String. Name of the field containing parent choices.
#' @param display_col String. Optional column name containing display text.
#' @param is_param_parent Logical. Whether this is a parameter parent field.
#'   Default: `FALSE`.
#' @param choice_ids Vector. Optional IDs to associate with choices.
#'
#' @return A list formatted for JavaScript survey components containing:
#'   - `type`: Field type ("parent", "param_parent", "child", or "standalone")
#'   - `choices`: List of formatted choices with value, text, and relationship data
#'   - Additional metadata fields depending on relationship type
#'
#' @examples
#' \dontrun{
#' # Parent choices
#' choices <- c("parent1", "parent2")
#' formatted <- format_choices_for_js(
#'   choices,
#'   is_parent = TRUE,
#'   child_field = "child_field"
#' )
#'
#' # Child choices with parent IDs
#' child_choices <- list(
#'   list(
#'     value = "child1",
#'     text = "Child 1",
#'     parentId = 1,
#'     parentValue = "parent1"
#'   ),
#'   list(
#'     value = "child2",
#'     text = "Child 2",
#'     parentId = 1,
#'     parentValue = "parent1"
#'   )
#' )
#' formatted_children <- format_choices_for_js(
#'   child_choices,
#'   is_child = TRUE,
#'   parent_field = "parent_field"
#' )
#' }
#'
#' @noRd
#' @keywords internal
format_choices_for_js <- function(choices,
                                  is_parent = FALSE,
                                  child_field = NULL,
                                  is_child = FALSE,
                                  parent_field = NULL,
                                  display_col = NULL,
                                  is_param_parent = FALSE,
                                  choice_ids = NULL) {
  if (is.null(choices)) {
    return(NULL)
  }

  # Create base structure based on relationship type
  base_structure <- if (is_parent) {
    list(
      type = if (is_param_parent) "param_parent" else "parent",
      choices = list()
    )
  } else if (is_child) {
    list(
      type = "child",
      parentField = parent_field,
      choices = list()
    )
  } else {
    list(
      type = "standalone",
      choices = list()
    )
  }

  # For child fields, structure the choices differently
  if (is_child) {
    if (is.list(choices[[1]]) && !is.null(choices[[1]]$parentId)) {
      # Extract parallel arrays for child choices
      values <- sapply(choices, function(x) x$value)
      texts <- sapply(choices, function(x) x$text)
      parent_ids <- sapply(choices, function(x) x$parentId)
      parent_values <- sapply(choices, function(x) x$parentValue)

      base_structure$choices <- list(
        value = unlist(values),
        text = unlist(texts),
        parentId = parent_ids,
        parentValue = parent_values
      )
    }
    return(base_structure)
  }

  # For parent or standalone fields
  if (is.character(choices) || is.factor(choices)) {
    choices <- as.character(choices)
    base_structure$choices <- list(
      value = choices,
      text = if (!is.null(display_col)) display_col else choices,
      ids = if (!is.null(choice_ids)) choice_ids else seq_along(choices)
    )
    if (child_field) {
      base_structure$childField <- child_field # Move childField to top level
    }
  }

  return(base_structure)
}

#' Configure Dynamic Fields for Survey
#'
#' Configures dynamic fields in a survey based on provided configuration parameters.
#' Handles choice population from database tables, parent-child field relationships,
#' parameter-based field values, and unique value validation. Sends formatted field
#' configurations to the client via custom messages.
#'
#' @param dynamic_config List. Configuration entries, where each entry contains:
#'   \describe{
#'     \item{config_type}{String. Field type: "choice", "param", or "unique"}
#'     \item{config_col}{String. Column name containing choice values}
#'     \item{parent_table_name}{String. Optional parent table for dependencies}
#'     \item{parent_id_col}{String. Optional column for parent-child relationships}
#'     \item{display_col}{String. Optional column for display text}
#'     \item{result}{String. For unique validation: "warn" or "stop"}
#'     \item{result_field}{String. Required for warnings: field to display message}
#'   }
#' @param config_list_reactive Reactive. Contains cached database tables.
#' @param session ShinySession. Session object for client communication.
#' @param logger Logger. Object for operation logging.
#' @param write_table String. Table name for unique value validation.
#' @param db_ops DBOps. Database operations object with table reading methods.
#'
#' @return Invisible NULL, called for side effects.
#'
#' @noRd
#' @keywords internal
configure_dynamic_fields <- function(dynamic_config, config_list_reactive, session, logger, write_table, db_ops) {
  # Initialize empty list for choices
  choices_data <- list()

  # Filter dynamic_config to only include configs relevant to the current survey
  # In multisurvey mode, write_table contains the survey name, so we filter by target_tbl
  relevant_configs <- if (!is.null(write_table)) {
    Filter(function(config) {
      target_tbl <- config$target_tbl
      # If no target_tbl specified, include the config (legacy behavior)
      # If target_tbl matches current survey (write_table), include it
      is.null(target_tbl) || target_tbl == write_table
    }, dynamic_config)
  } else {
    dynamic_config
  }
  
  if (length(relevant_configs) == 0) {
    logger$log_message("No dynamic configs applicable to current survey", "INFO", "SURVEY")
    return(invisible(NULL))
  }
  
  logger$log_message(
    sprintf("Processing %d dynamic configs for survey '%s'", 
           length(relevant_configs), write_table %||% "default"),
    "INFO", "SURVEY"
  )

  # Process each relevant dynamic config entry
  for (config in relevant_configs) {
    # Support both new and legacy field names
    config_type <- config$type %||% config$config_type
    target_col <- config$target_col %||% config$config_col
    source_tbl <- config$source_tbl %||% config$table_name
    
    if (config_type == "choice") {
      tryCatch({
        # Get source table data
        source_data <- config_list_reactive[[source_tbl]]
        if (is.null(source_data)) {
          logger$log_message(sprintf("Source table '%s' not found", source_tbl), "ERROR", "SURVEY")
          next
        }
        
        # Apply filter_source if specified
        if (!is.null(config$filter_source)) {
          # Parse and apply the filter (simplified SQL-like filter)
          filter_expr <- config$filter_source
          
          # Handle simple equality filters like "closed == 0"
          if (grepl("==", filter_expr)) {
            parts <- strsplit(gsub("\\s", "", filter_expr), "==")[[1]]
            if (length(parts) == 2) {
              col_name <- parts[1]
              filter_value <- as.numeric(parts[2])
              
              if (col_name %in% names(source_data)) {
                source_data <- source_data[source_data[[col_name]] == filter_value, , drop = FALSE]
                logger$log_message(
                  sprintf("Applied filter '%s' to source table, %d rows remaining", 
                         filter_expr, nrow(source_data)),
                  "INFO", "SURVEY"
                )
              }
            }
          }
        }
        
        # Get choices from source data
        source_col <- config$source_col %||% target_col
        source_display_col <- config$source_display_col %||% source_col
        
        if (!source_col %in% names(source_data)) {
          logger$log_message(sprintf("Source column '%s' not found in table '%s'", source_col, source_tbl), "ERROR", "SURVEY")
          next
        }
        
        choice_values <- source_data[[source_col]]
        choice_texts <- if (source_display_col %in% names(source_data)) {
          source_data[[source_display_col]]
        } else {
          choice_values
        }
        
        # Apply filter_unique if specified
        if (isTRUE(config$filter_unique) && !is.null(config$target_tbl)) {
          # Check what values have already been submitted in the target table
          # Use the target_tbl from config, which should match the survey name in multisurvey mode
          target_tbl <- config$target_tbl
          
          existing_values <- tryCatch({
            existing_data <- db_ops$read_table(target_tbl)
            if (target_col %in% names(existing_data)) {
              existing_data[[target_col]]
            } else {
              character(0)
            }
          }, error = function(e) {
            logger$log_message(sprintf("Error reading target table '%s': %s", target_tbl, e$message), "WARN", "SURVEY")
            character(0)
          })
          
          # Filter out already used values
          if (length(existing_values) > 0) {
            keep_indices <- !choice_values %in% existing_values
            choice_values <- choice_values[keep_indices]
            choice_texts <- choice_texts[keep_indices]
            
            logger$log_message(
              sprintf("Applied unique filter, %d choices remaining after removing %d used values", 
                     length(choice_values), sum(!keep_indices)),
              "INFO", "SURVEY"
            )
          }
        }
        
        # Remove NA values and ensure we have valid choices
        valid_indices <- !is.na(choice_values) & !is.na(choice_texts)
        choice_values <- choice_values[valid_indices]
        choice_texts <- choice_texts[valid_indices]
        
        if (length(choice_values) > 0) {
          # Create the choice data structure - ensure values are arrays/vectors
          choice_data <- list(
            type = "standalone",
            target_tbl = config$target_tbl,
            choices = list(
              value = as.vector(choice_values),
              text = as.vector(choice_texts)
            )
          )
          
          choices_data[[target_col]] <- choice_data
          
          logger$log_message(
            sprintf("Created choice data for '%s' with %d options", target_col, length(choice_values)),
            "INFO", "SURVEY"
          )
        } else {
          logger$log_message(
            sprintf("No valid choices found for field '%s'", target_col),
            "WARN", "SURVEY"
          )
        }
        
      }, error = function(e) {
        logger$log_message(sprintf("Error processing choice config for '%s': %s", target_col, e$message), "ERROR", "SURVEY")
      })
      
    } else if (config_type == "param") {
      # Handle param type (legacy behavior)
      tryCatch({
        table_data <- config_list_reactive[[source_tbl]]
        if (!is.null(table_data) && target_col %in% names(table_data)) {
          choices <- unique(table_data[[target_col]])
          choices <- choices[!is.na(choices)]
          
          if (length(choices) > 0) {
            choices_data[[target_col]] <- list(
              type = "param",
              choices = list(
                value = as.vector(choices),
                text = as.vector(choices)
              )
            )
          }
        }
      }, error = function(e) {
        logger$log_message(sprintf("Error processing param config: %s", e$message), "ERROR", "SURVEY")
      })
    }
  }

  # Process unique validation fields
  unique_fields <- tryCatch({
    get_unique_field_values(
      dynamic_config = relevant_configs,
      db_ops = db_ops,
      write_table = write_table
    )
  }, error = function(e) {
    logger$log_message(
      sprintf("Error getting unique field values: %s", e$message),
      "ERROR", "SURVEY"
    )
    list()
  })

  if (length(unique_fields) > 0) {
    choices_data$unique_validation <- unique_fields
    logger$log_message(
      sprintf("Added unique validation for %d fields", length(unique_fields)),
      "INFO", "SURVEY"
    )
  }

  # Send data to the client if we have data
  has_data <- length(unique_fields) > 0 || 
    length(setdiff(names(choices_data), "unique_validation")) > 0

  if (has_data) {
    tryCatch({
      session$sendCustomMessage("updateDynamicChoices", as.list(choices_data))
      logger$log_message(
        sprintf("Sent data to frontend with %d field configurations", 
               length(setdiff(names(choices_data), "unique_validation"))),
        "INFO", "SURVEY"
      )
    }, error = function(e) {
      logger$log_message(sprintf("Error sending data: %s", e$message), "ERROR", "SURVEY")
    })
  } else {
    logger$log_message("No valid choices found for this survey", "ERROR", "SURVEY")
    # Use the existing hide_and_show pattern to show error message
    tryCatch({
      hide_and_show("waitingMessage", "surveyNotDefinedMessage")
    }, error = function(e) {
      logger$log_message(sprintf("Error showing error message: %s", e$message), "ERROR", "SURVEY")
    })
  }

  invisible(NULL)
}

#' Normalize Field Value for Comparison
#'
#' Takes a character value and normalizes it for consistent comparison by converting
#' to lowercase, trimming whitespace, removing multiple spaces, and optionally
#' removing special characters.
#'
#' @param value Character. Value to normalize.
#' @param remove_special Logical. Remove special characters. Default: `FALSE`.
#'
#' @return Character. Normalized string value.
#'
#' @noRd
#' @keywords internal
normalize_field_value <- function(value, remove_special = TRUE) {
  if (is.null(value) || !is.character(value)) {
    return(value)
  }

  # Convert to lowercase
  normalized <- tolower(value)

  # Trim whitespace and collapse multiple spaces
  normalized <- gsub("\\s+", " ", trimws(normalized))

  if (remove_special) {
    # Remove special characters, keeping only alphanumeric and space
    normalized <- gsub("[^[:alnum:]\\s]", "", normalized)
  }

  return(normalized)
}

#' Validate Dynamic Configuration
#'
#' Validates survey field dynamic configuration structure, checking configuration types,
#' required parameters, and database table references. Provides detailed validation
#' results and caches validated tables.
#'
#' @param dynamic_config List. Dynamic field configurations to validate with required
#'   elements:
#'   * `config_type`: Configuration type ("choice", "param", or "unique")
#'   * Additional type-specific parameters as defined in survey documentation
#' @param config_list List. Pre-cached configuration tables for validation.
#'   Default: `NULL`.
#' @param survey_logger Logger. Logging object for validation messages.
#'
#' @return Named list:
#'   * `valid`: Logical. `TRUE` if configuration passes all checks
#'   * `errors`: Character. Validation error messages if any
#'   * `cached_tables`: List. Validated and cached database tables
#'
#' @noRd
#' @keywords internal
validate_dynamic_config <- function(dynamic_config, config_list = NULL, survey_logger = NULL) {
  errors <- character()

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

    # Check if config is a list
    if (!is.list(config)) {
      error_msg <- paste0(prefix, "must be a list")
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
      next
    }

    # Check required fields - support both new and legacy field names
    # New API uses 'type' and 'target_col', legacy uses 'config_type' and 'config_col'
    config_type_field <- config$type %||% config$config_type
    config_col_field <- config$target_col %||% config$config_col
    
    if (is.null(config_type_field)) {
      error_msg <- paste0(prefix, "missing required field: 'type' (or legacy 'config_type')")
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
    }
    
    if (is.null(config_col_field)) {
      error_msg <- paste0(prefix, "missing required field: 'target_col' (or legacy 'config_col')")
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
    }

    # Check config_type validity
    if (!is.null(config_type_field)) {
      if (!config_type_field %in% c("choice", "param", "unique")) {
        error_msg <- paste0(prefix, "type must be 'choice', 'param', or 'unique'")
        errors <- c(errors, error_msg)
        if (!is.null(survey_logger)) {
          survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
        }
      }

      # Additional validation for unique type
      if (config_type_field == "unique") {
        # Validate required fields for unique validation
        unique_required <- c("result")
        missing_unique <- unique_required[!unique_required %in% names(config)]

        if (length(missing_unique) > 0) {
          error_msg <- paste0(
            prefix, "unique validation missing required fields: ",
            paste(missing_unique, collapse = ", ")
          )
          errors <- c(errors, error_msg)
        }

        # Validate result type
        if ("result" %in% names(config) &&
          !config$result %in% c("warn", "stop")) {
          error_msg <- paste0(prefix, "result must be either 'warn' or 'stop'")
          errors <- c(errors, error_msg)
        }

        # If warn type, must have result_field
        if ("result" %in% names(config) &&
          config$result == "warn" &&
          !"result_field" %in% names(config)) {
          error_msg <- paste0(prefix, "warn result type requires result_field")
          errors <- c(errors, error_msg)
        }
      }
    }
  }

  # Return validation results
  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

#' Get Existing Values for Unique Fields
#'
#' Retrieves existing values from the database for fields that require uniqueness
#' validation according to a dynamic configuration.
#'
#' @param dynamic_config List. Configuration specifying fields requiring unique values.
#'   Each entry should contain:
#'   * `config_type`: Must be "unique"
#'   * `config_col`: Column to check for uniqueness
#' @param db_ops Object. Database operations handler containing read methods.
#' @param write_table String. Name of the database table to check values against.
#'
#' @return Named list mapping field names to vectors of their existing values
#'   in the database.
#'
#' @noRd
#' @keywords internal
get_unique_field_values <- function(dynamic_config, db_ops, write_table) {
  if (is.null(db_ops)) {
    warning("Database operations not initialized")
    return(list())
  }

  if (is.null(write_table) || nchar(write_table) == 0) {
    warning("Write table not specified")
    return(list())
  }

  unique_configs <- Filter(function(config) {
    config_type <- config$type %||% config$config_type
    isTRUE(config_type == "unique")
  }, dynamic_config)

  if (length(unique_configs) == 0) {
    return(list())
  }

  table_exists <- tryCatch(
    {
      db_ops$operate(
        function(conn) DBI::dbExistsTable(conn, write_table),
        "Failed to check table existence"
      )
    },
    error = function(e) {
      warning("Error checking table existence: ", e$message)
      return(FALSE)
    }
  )

  if (!isTRUE(table_exists)) {
    return(list())
  }

  values <- lapply(unique_configs, function(config) {
    field_name <- config$target_col %||% config$config_col

    existing <- tryCatch(
      {
        db_ops$read_table(
          table_name = write_table,
          columns = c(field_name)
        )
      },
      error = function(e) {
        warning(paste("Error reading table for field", field_name, ":", e$message))
        return(data.frame())
      }
    )

    if (nrow(existing) > 0) {
      original_values <- existing[[field_name]]

      # Create normalized values as a list instead of a named vector
      normalized_values <- lapply(original_values, function(val) {
        normalized <- normalize_field_value(val, remove_special = TRUE)
        list(
          original = val,
          normalized = normalized
        )
      })

      return(list(
        values = as.list(original_values),
        normalized_values = normalized_values,
        result = config$result,
        result_field = config$result_field,
        normalization_settings = list(
          remove_special = TRUE
        )
      ))
    }

    return(NULL)
  })

  names(values) <- vapply(unique_configs, function(x) x$target_col %||% x$config_col, character(1))

  return(values)
}
