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
#'   - table_name: Name of the database table
#'   - group_type: Must be either "choice" or "param"
#'   - group_col: Column name for grouping
#'   - parent_table_name: (Optional) Name of parent table for relationships
#'   - parent_id_col: (Optional) Column name in parent table for relationship
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
#'     parent_table_name = "package_versions",
#'     parent_id_col = "package_id"
#'   ),
#'   list(
#'     table_name = "config_pid",
#'     group_type = "param",
#'     group_col = "pid",
#'     display_col = "full_name",
#'     parent_table_name = "projects",
#'     parent_id_col = "project_id"
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
#'
#' @return A list formatted for JavaScript survey components with relationship metadata
#'
#' @examples
#' \dontrun{
#' # Parent choices
#' choices <- c("parent1", "parent2")
#' formatted <- format_choices_for_js(choices, is_parent = TRUE, child_field = "child_field")
#'
#' # Child choices
#' child_choices <- list(
#'   list(value = "child1", text = "Child 1", parentId = 1),
#'   list(value = "child2", text = "Child 2", parentId = 1)
#' )
#' formatted_children <- format_choices_for_js(child_choices, is_child = TRUE, parent_field = "parent_field")
#' }
format_choices_for_js <- function(choices,
                                  is_parent = FALSE,
                                  child_field = NULL,
                                  is_child = FALSE,
                                  parent_field = NULL,
                                  display_col = NULL) {
  if (is.null(choices)) return(NULL)

  # Create base structure based on relationship type
  base_structure <- if (is_parent) {
    list(
      type = "parent",
      choices = list()
    )
  } else if (is_child) {
    list(
      type = "child",
      parentField = parent_field,
      choices = list()
    )
  } else {
    list(choices = list())
  }

  # Handle flat choices with optional display text
  if (!is.list(choices) || (is.list(choices) && !is.null(names(choices)) &&
                            !all(c("value", "text", "choices") %in% names(choices[[1]])))) {
    # Convert choices to array of objects format
    choices_list <- vector("list", length(choices))
    for(i in seq_along(choices)) {
      choice_struct <- list(
        value = choices[i],
        text = if (!is.null(display_col) && !is.null(display_col[i])) display_col[i] else choices[i]
      )

      if (is_parent && !is.null(child_field)) {
        choice_struct$childField <- child_field
      }

      choices_list[[i]] <- choice_struct
    }

    base_structure$choices <- choices_list
    return(base_structure)
  }

  # Handle hierarchical choices
  choices_list <- vector("list", length(choices))
  for(i in seq_along(choices)) {
    item <- choices[[i]]

    if (is_child && is.list(item$value)) {
      # Handle array values for child choices
      choice_struct <- list(
        value = unlist(item$value),
        text = unlist(item$value),  # or item$text if available
        parentId = item$parentId
      )
    } else {
      choice_struct <- list(
        value = item$value,
        text = if (!is.null(item$display_text)) item$display_text else item$text
      )

      if (is_parent && !is.null(child_field)) {
        choice_struct$childField <- child_field
      }

      if (is_child && !is.null(item$parentId)) {
        choice_struct$parentId <- item$parentId
      }
    }

    choices_list[[i]] <- choice_struct
  }

  base_structure$choices <- choices_list
  return(base_structure)
}

#' Configure Dynamic Fields for Survey
#'
#' @description
#' Configures dynamic fields based on the provided configuration, handling both
#' choices and parameters. Supports parent-child relationships between fields and
#' optional display text for choices.
#'
#' @param dynamic_config List of configuration entries. Each entry must be a list containing:
#'   - group_type: Character, either "choice" or "param"
#'   - table_name: Character, name of the source table
#'   - group_col: Character, name of the column containing choices
#'   - parent_table_name: (Optional) Character, name of parent table
#'   - parent_id_col: (Optional) Character, column name for parent-child relationship
#'   - display_col: (Optional) Character, column name containing display text
#'
#' @param config_list_reactive Reactive expression containing cached database tables
#' @param session Shiny session object
#' @param logger Logger object
#'
#' @return Invisible NULL
configure_dynamic_fields <- function(dynamic_config, config_list_reactive, session, logger) {
  # Validate inputs
  if (is.null(dynamic_config) || !is.list(dynamic_config)) {
    logger$log_message("Invalid dynamic_config parameter", type = "ERROR", zone = "SURVEY")
    return(invisible(NULL))
  }

  # Extract choice configurations
  choice_configs <- Filter(function(x) {
    !is.null(x$group_type) && x$group_type == "choice"
  }, dynamic_config)

  if (length(choice_configs) == 0) {
    logger$log_message("No choice configurations found", type = "INFO", zone = "SURVEY")
    return(invisible(NULL))
  }

  # Process each choice configuration
  choices_data <- list()

  for (config in choice_configs) {
    tryCatch({
      # Validate required fields
      if (is.null(config$table_name) || is.null(config$group_col)) {
        logger$log_message(
          sprintf("Missing required fields in config: %s",
                  jsonlite::toJSON(config)),
          type = "ERROR",
          zone = "SURVEY"
        )
        next
      }

      # Get the specific table and column
      table_data <- config_list_reactive[[config$table_name]]
      col_name <- config$group_col

      if (is.null(table_data)) {
        logger$log_message(
          sprintf("Table '%s' not found in config_list", config$table_name),
          type = "ERROR",
          zone = "SURVEY"
        )
        next
      }

      if (!col_name %in% names(table_data)) {
        logger$log_message(
          sprintf("Column '%s' not found in table '%s'",
                  col_name, config$table_name),
          type = "ERROR",
          zone = "SURVEY"
        )
        next
      }

      # Basic scenario: direct choices from column
      if (is.null(config$parent_table_name)) {
        choices <- unique(table_data[[col_name]])
        choices <- choices[!is.na(choices)]

        if (length(choices) > 0) {
          # Check if this field is referenced as a parent in other configs
          is_parent <- any(sapply(choice_configs, function(other_config) {
            !is.null(other_config$parent_table_name) &&
              other_config$parent_table_name == config$table_name
          }))

          child_field <- if (is_parent) {
            # Find the child field name
            child_config <- Find(function(other_config) {
              !is.null(other_config$parent_table_name) &&
                other_config$parent_table_name == config$table_name
            }, choice_configs)
            child_config$group_col
          } else {
            NULL
          }

          display_text <- if (!is.null(config$display_col)) {
            table_data[[config$display_col]][!is.na(table_data[[col_name]])]
          } else {
            NULL
          }

          choices_data[[col_name]] <- format_choices_for_js(
            choices,
            is_parent = is_parent,
            child_field = child_field,
            display_col = display_text
          )

          logger$log_message(
            sprintf("Added %d choices for field '%s'",
                    length(choices), col_name),
            type = "INFO",
            zone = "SURVEY"
          )
        }
      }

      # Parent-child relationship scenario
      if (!is.null(config$parent_table_name) && !is.null(config$parent_id_col)) {
        parent_table <- config_list_reactive[[config$parent_table_name]]

        if (is.null(parent_table)) {
          logger$log_message(
            sprintf("Parent table '%s' not found", config$parent_table_name),
            type = "ERROR",
            zone = "SURVEY"
          )
          next
        }

        if (!config$parent_id_col %in% names(parent_table)) {
          logger$log_message(
            sprintf("Parent ID column '%s' not found in table '%s'",
                    config$parent_id_col, config$parent_table_name),
            type = "ERROR",
            zone = "SURVEY"
          )
          next
        }

        # Create choices with parent IDs
        choices <- lapply(unique(table_data[[config$parent_id_col]]), function(parent_id) {
          child_choices <- table_data[[col_name]][table_data[[config$parent_id_col]] == parent_id]
          child_choices <- child_choices[!is.na(child_choices)]

          if (length(child_choices) > 0) {
            choice_struct <- list(
              value = child_choices,
              text = child_choices
            )

            if (!is.null(config$display_col)) {
              display_text <- table_data[[config$display_col]][
                table_data[[config$parent_id_col]] == parent_id &
                  !is.na(table_data[[col_name]])
              ]
              choice_struct$display_text <- display_text
            }

            choice_struct$parentId <- parent_id
            choice_struct
          }
        })

        choices <- Filter(Negate(is.null), choices)

        if (length(choices) > 0) {
          choices_data[[col_name]] <- format_choices_for_js(
            choices,
            is_child = TRUE,
            parent_field = config$parent_id_col
          )

          logger$log_message(
            sprintf("Added child choices for field '%s'", col_name),
            type = "INFO",
            zone = "SURVEY"
          )
        }
      }

    }, error = function(e) {
      logger$log_message(
        sprintf("Error processing config: %s", e$message),
        type = "ERROR",
        zone = "SURVEY"
      )
    })
  }

  # Send choices to the client if we have any
  if (length(choices_data) > 0) {
    tryCatch({
      json_data <- jsonlite::toJSON(choices_data, auto_unbox = TRUE)

      # print(json_data)

      session$sendCustomMessage("updateDynamicChoices", jsonlite::fromJSON(json_data))

      logger$log_message(
        sprintf("Sent %d dynamic choice configurations to client",
                length(choices_data)),
        type = "INFO",
        zone = "SURVEY"
      )
    }, error = function(e) {
      logger$log_message(
        sprintf("Failed to send choices to client: %s", e$message),
        type = "ERROR",
        zone = "SURVEY"
      )
    })
  } else {
    logger$log_message(
      "No valid choices data to send to client",
      type = "WARN",
      zone = "SURVEY"
    )
  }

  invisible(NULL)
}
