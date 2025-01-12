#' Read and Cache Tables for Dynamic Fields
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
#' # Define dynamic fields configuration
#' config <- list(
#'   list(
#'     group_type = "choice",
#'     table_name = "config_packages",
#'     group_col = "package"
#'   ),
#'   list(
#'     group_type = "param",
#'     table_name = "config_source",
#'     group_col = "source",
#'     display_col = "display_text"
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
#' @keywords internal
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
#'   - **table_name**: Name of the database table
#'   - **group_type**: Must be either "choice" or "param"
#'   - **group_col**: Column name for grouping
#'   - **parent_table_name**: (Optional) Name of parent table for relationships
#'   - **parent_id_col**: (Optional) Column name in parent table for relationship
#'   - **display_col**: (Required for group_type="param") Column name for display
#'
#' @param config_list Optional list of cached tables to verify table/column existence
#' @param survey_logger Logger object for logging validation results and errors
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
#'     group_col = "package"
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
#' result <- validate_dynamic_config(config, config_list, survey_logger)
#' if (!result$valid) {
#'   stop(paste(result$errors, collapse = "\n"))
#' }
#' }
#'
#' @keywords internal
validate_dynamic_config <- function(dynamic_config, config_list = NULL, survey_logger = NULL) {
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
        sprintf("Checking configuration %d: '%s'", i, config$table_name),
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
      }
    }

    # Check parent table configuration if provided
    if (("parent_table_name" %in% names(config) && !"parent_id_col" %in% names(config)) ||
        (!"parent_table_name" %in% names(config) && "parent_id_col" %in% names(config))) {
      error_msg <- paste0(prefix, "both parent_table_name and parent_id_col must be provided together")
      errors <- c(errors, error_msg)
      if (!is.null(survey_logger)) {
        survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
      }
    }

    # If config_list is provided, verify table and column existence
    if (!is.null(config_list) && "table_name" %in% names(config)) {
      # Check if main table exists in cache
      if (!config$table_name %in% names(config_list)) {
        error_msg <- paste0(prefix, "table '", config$table_name, "' not found in cache")
        errors <- c(errors, error_msg)
        if (!is.null(survey_logger)) {
          survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
        }
      } else {
        table_data <- config_list[[config$table_name]]

        # Check if columns exist in main table
        cols_to_check <- c(config$group_col)
        if ("display_col" %in% names(config)) {
          cols_to_check <- c(cols_to_check, config$display_col)
        }

        missing_cols <- cols_to_check[!cols_to_check %in% names(table_data)]
        if (length(missing_cols) > 0) {
          error_msg <- paste0(prefix, "columns not found in table: ",
                              paste(missing_cols, collapse = ", "))
          errors <- c(errors, error_msg)
          if (!is.null(survey_logger)) {
            survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
          }
        }

        # Check parent table configuration if provided
        if ("parent_table_name" %in% names(config)) {
          if (!config$parent_table_name %in% names(config_list)) {
            error_msg <- paste0(prefix, "parent table '", config$parent_table_name, "' not found in cache")
            errors <- c(errors, error_msg)
            if (!is.null(survey_logger)) {
              survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
            }
          } else {
            parent_table_data <- config_list[[config$parent_table_name]]
            if (!config$parent_id_col %in% names(parent_table_data)) {
              error_msg <- paste0(prefix, "parent_id_col '", config$parent_id_col,
                                  "' not found in parent table '", config$parent_table_name, "'")
              errors <- c(errors, error_msg)
              if (!is.null(survey_logger)) {
                survey_logger$log_message(error_msg, type = "ERROR", zone = "SURVEY")
              }
            }
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
#'
#' @keywords internal
get_source_display_text <- function(source_value, config_source_df) {
  display_text <- config_source_df$display_text[config_source_df$source == source_value]
  if(length(display_text) == 0) return(source_value)  # fallback to value if not found
  return(display_text)
}

#' Transform Validated Parameters
#'
#' @description
#' Transforms a list of validated parameters into a structured format with text/value pairs,
#' looking up display text from configuration tables where applicable.
#'
#' @param validated_params List. Input parameters in the format:
#'   list(param_name = "value") or list(param_name = list(value = "value"))
#' @param config_list List. Configuration data containing tables including:
#'   - **config_source**: Data frame with columns 'source' and 'display_text'
#'   - **config_packages**: Data frame with package configurations
#'
#' @return List. Transformed parameters in the format:
#'   list(param_name = list(text = "display_text", value = "value"))
#'
#' @keywords internal
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

#' Format Choices for JavaScript Survey Library
#'
#' @description
#' Formats R choice data structures into a format compatible with JavaScript survey components.
#' Handles both flat lists of choices and hierarchical choice structures, with support for
#' parent-child relationships between fields.
#'
#' @param choices Either a vector of choices or a list of hierarchical choices
#' @param is_parent Logical, whether this field is a parent field
#' @param child_field Character, name of the field containing child choices
#' @param is_child Logical, whether this field contains child choices
#' @param parent_field Character, name of the field containing parent choices
#' @param display_col Character, optional name of column containing display text
#' @param is_param_parent Logical, whether this is a parameter parent field
#' @param choice_ids Vector, optional IDs to associate with choices
#'
#' @return A list formatted for JavaScript survey components containing:
#'   - **type**: Type of field ("parent", "param_parent", "child", or "standalone")
#'   - **choices**: List of formatted choices with value, text, and relationship data
#'   - Additional metadata fields depending on relationship type
#'
#' @examples
#' \dontrun{
#' # Parent choices
#' choices <- c("parent1", "parent2")
#' formatted <- format_choices_for_js(choices, is_parent = TRUE, child_field = "child_field")
#'
#' # Child choices with parent IDs
#' child_choices <- list(
#'   list(value = "child1", text = "Child 1", parentId = 1, parentValue = "parent1"),
#'   list(value = "child2", text = "Child 2", parentId = 1, parentValue = "parent1")
#' )
#' formatted_children <- format_choices_for_js(
#'   child_choices,
#'   is_child = TRUE,
#'   parent_field = "parent_field"
#' )
#' }
#'
#' @keywords internal
format_choices_for_js <- function(choices,
                                  is_parent = FALSE,
                                  child_field = NULL,
                                  is_child = FALSE,
                                  parent_field = NULL,
                                  display_col = NULL,
                                  is_param_parent = FALSE,
                                  choice_ids = NULL) {
  if (is.null(choices)) return(NULL)

  # Create base structure based on relationship type
  base_structure <- if (is_parent) {
    list(
      type = if(is_param_parent) "param_parent" else "parent",
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
      base_structure$childField <- child_field  # Move childField to top level
    }
  }

  return(base_structure)
}

#' Configure Dynamic Fields for Survey
#'
#' @description
#' Configures dynamic fields based on the provided configuration, handling both
#' choices and parameters. Supports parent-child relationships between fields and
#' optional display text for choices. Sends the formatted field configurations
#' to the client via a custom message.
#'
#' @param dynamic_config List of configuration entries. Each entry must be a list containing:
#'   - **group_type**: Character, either "choice" or "param"
#'   - **table_name**: Character, name of the source table
#'   - **group_col**: Character, name of the column containing choices
#'   - **parent_table_name**: (Optional) Character, name of parent table
#'   - **parent_id_col**: (Optional) Character, column name for parent-child relationship
#'   - display_col: (Optional) Character, column name containing display text
#' @param config_list_reactive Reactive expression containing cached database tables
#' @param session Shiny session object for sending messages to the client
#' @param logger Logger object for recording operation results
#'
#' @return Invisible NULL. The function operates via side effects, sending formatted
#'   field configurations to the client via `session$sendCustomMessage`.
#'
#' @keywords internal
configure_dynamic_fields <- function(dynamic_config, config_list_reactive, session, logger) {
  # Validate inputs
  if (is.null(dynamic_config) || !is.list(dynamic_config)) {
    logger$log_message("Invalid dynamic_config parameter", type = "ERROR", zone = "SURVEY")
    return(invisible(NULL))
  }

  # Process each choice configuration
  choices_data <- list()

  # First pass: Process parent fields (both param and choice types)
  for (config in dynamic_config) {
    if (config$group_type %in% c("param", "choice")) {
      # Check if this field is referenced as a parent
      is_parent <- any(sapply(dynamic_config, function(other_config) {
        !is.null(other_config$parent_table_name) &&
          other_config$parent_table_name == config$table_name
      }))

      if (is_parent) {
        tryCatch({
          # Get table data
          table_data <- config_list_reactive[[config$table_name]]
          if (is.null(table_data)) {
            logger$log_message(sprintf("Table '%s' not found", config$table_name), "ERROR", "SURVEY")
            next
          }

          # Find child config
          child_config <- Find(function(other_config) {
            !is.null(other_config$parent_table_name) &&
              other_config$parent_table_name == config$table_name
          }, dynamic_config)

          if (is.null(child_config)) {
            logger$log_message("No child configuration found", "ERROR", "SURVEY")
            next
          }

          # Create parent field data
          parent_data <- list(
            type = if(config$group_type == "param") "param_parent" else "choice_parent",
            childField = child_config$group_col,
            choices = list(
              value = table_data[[config$group_col]],
              text = if(!is.null(config$display_col)) {
                table_data[[config$display_col]]
              } else {
                table_data[[config$group_col]]
              },
              ids = table_data$package_id
            )
          )

          choices_data[[config$group_col]] <- parent_data

          logger$log_message(
            sprintf("Created parent data for '%s'", config$group_col),
            "INFO",
            "SURVEY"
          )

        }, error = function(e) {
          logger$log_message(sprintf("Error in parent processing: %s", e$message), "ERROR", "SURVEY")
        })
      } else if (config$group_type == "choice") {
        # Handle standalone choice fields (no parent/child relationship)
        tryCatch({
          table_data <- config_list_reactive[[config$table_name]]
          if (!is.null(table_data)) {
            choices <- unique(table_data[[config$group_col]])
            choices <- choices[!is.na(choices)]

            if (length(choices) > 0) {
              choices_data[[config$group_col]] <- list(
                type = "standalone",
                choices = list(
                  value = choices,
                  text = if(!is.null(config$display_col)) {
                    table_data[[config$display_col]][!is.na(choices)]
                  } else {
                    choices
                  }
                )
              )
            }
          }
        }, error = function(e) {
          logger$log_message(sprintf("Error in standalone choice processing: %s", e$message),
                             "ERROR", "SURVEY")
        })
      }
    }
  }

  # Second pass: Process child fields
  for (config in dynamic_config) {
    if (config$group_type == "choice" &&
        !is.null(config$parent_table_name) &&
        !is.null(config$parent_id_col)) {
      tryCatch({
        # Get tables
        child_table <- config_list_reactive[[config$table_name]]
        parent_table <- config_list_reactive[[config$parent_table_name]]

        if (is.null(child_table) || is.null(parent_table)) {
          logger$log_message("Missing required tables", "ERROR", "SURVEY")
          next
        }

        # Create child field data
        child_data <- list(
          type = "child",
          parentField = config$parent_id_col,  # Use the actual parent field
          choices = list(
            value = child_table[[config$group_col]],
            text = child_table[[config$group_col]],
            parentId = child_table[[config$parent_id_col]],
            parentValue = parent_table[[config$group_col]][match(
              child_table[[config$parent_id_col]],
              parent_table[[config$parent_id_col]]
            )]
          )
        )

        choices_data[[config$group_col]] <- child_data

        logger$log_message(
          sprintf("Created child data for '%s'", config$group_col),
          "INFO",
          "SURVEY"
        )

      }, error = function(e) {
        logger$log_message(sprintf("Error in child processing: %s", e$message), "ERROR", "SURVEY")
      })
    }
  }

  # Send choices to the client
  if (length(choices_data) > 0) {
    tryCatch({
      session$sendCustomMessage("updateDynamicChoices", choices_data)

      logger$log_message(
        sprintf("Sent JSON to frontend: %s", jsonlite::toJSON(choices_data)),
        "INFO",
        "SURVEY"
      )
    }, error = function(e) {
      logger$log_message(sprintf("Error sending choices: %s", e$message), "ERROR", "SURVEY")
    })
  } else {
    logger$log_message("No choices data to send", "WARN", "SURVEY")
  }

  invisible(NULL)
}
