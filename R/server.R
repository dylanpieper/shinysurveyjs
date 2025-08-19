#' Deploy a Survey Shiny Application
#'
#' Creates and deploys a Shiny application for a survey using SurveyJS
#' (<https://surveyjs.io>) with MySQL and DBI database integration. The application handles
#' survey data collection, dynamic fields, and asynchronous logging through a future plan.
#'
#' @param json String. JSON survey definition or object.
#' @param list List. Survey structure to convert to JSON.
#' @param show_response Logical. Display responses in a data.table after submission.
#'   Default: `FALSE`.
#' @param theme String. SurveyJS theme, either "defaultV2" or "modern".
#'   Default: "defaultV2".
#' @param theme_color String. Hex color code for primary theme customization.
#' @param shiny_config List. Optional Shiny configuration parameters.
#' @param db_config List. Database connection parameters. If not specified,
#'   values are read from environment variables:
#'   * `host`: Database host (env: HOST)
#'   * `port`: Database port (env: PORT)
#'   * `db_name`: Database name (env: DB_NAME)
#'   * `user`: Database username (env: USER)
#'   * `password`: Database password (env: PASSWORD)
#'   * `write_table`: Survey data table name (env: WRITE_TABLE)
#'   * `log_table`: Log messages table name (env: LOG_TABLE)
#' @param db_update List. Configuration for updating existing records instead of inserting new ones.
#'   Each element should be a list containing:
#'   * `from`: Source survey name to update from
#'   * `to`: Target survey name to update to
#'   * `by`: Named character vector specifying the join columns (e.g., c("field_in_from" = "field_in_to"))
#' @param db_logic List. Configuration for database logic. Supports three types:
#'   \subsection{Choice Configuration}{
#'     Populates dropdown or radio button choices from database tables:
#'     * `config_type`: Set to "choice"
#'     * `table_name`: Database table to populate choices from
#'     * `config_col`: Column containing choice text
#'     * `display_col`: Optional column for display text
#'
#'     For dependent fields:
#'     * `parent_table_name`: Parent table for dependency chain
#'     * `parent_id_col`: Column linking to parent table
#'   }
#'   \subsection{Parameter Configuration}{
#'     Handles URL query parameters and hidden fields:
#'     * `config_type`: Set to "param"
#'     * `table_name`: Database table with valid parameters
#'     * `config_col`: Column matching URL parameter name
#'     * `display_col`: Optional column for display text
#'   }
#'   \subsection{Unique Value Configuration}{
#'     Validates unique entries against existing database records:
#'     * `config_type`: Set to "unique"
#'     * `config_col`: Column to check for uniqueness
#'     * `result`: Action on duplicate ("warn" or "stop")
#'     * `result_field`: Survey field for warning message (should be hidden)
#'   }
#' @param cookie_expiration_days Numeric. Number of days to retain survey cookies (experimental).
#'   Default: 0.
#' @param custom_css String. Custom CSS rules to append to the theme.
#' @param suppress_logs Logical. Suppress console log messages. Default: `FALSE`.
#'
#' @return A Shiny application object
#'
#' @examples
#' \dontrun{
#' # Choice configuration example
#' db_logic <- list(
#'   list(
#'     config_type = "choice",
#'     table_name = "packages",
#'     config_col = "name"
#'   )
#' )
#'
#' # Parameter configuration example
#' db_logic <- list(
#'   list(
#'     config_type = "param",
#'     table_name = "sources",
#'     config_col = "source",
#'     display_col = "display_text"
#'   )
#' )
#'
#' # Unique value configuration example
#' db_logic <- list(
#'   list(
#'     config_type = "unique",
#'     config_col = "title",
#'     result = "warn",
#'     result_field = "warning_message"
#'   )
#' )
#' }
#'
#' @importFrom shiny fluidPage observeEvent reactive reactiveValues req outputOptions shinyApp div h1 h3 h4 tags
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom shinyjs hide show useShinyjs
#' @importFrom future plan multisession
#'
#' @export
survey <- function(json = NULL,
                   list = NULL,
                   show_response = FALSE,
                   theme = "defaultV2",
                   theme_color = "#003594",
                   shiny_config = list(
                     host = "0.0.0.0",
                     port = 3838
                   ),
                   db_config = list(
                     host = Sys.getenv("HOST"),
                     port = as.numeric(Sys.getenv("PORT")),
                     db_name = Sys.getenv("DB_NAME"),
                     user = Sys.getenv("USER"),
                     password = Sys.getenv("PASSWORD"),
                     write_table = Sys.getenv("WRITE_TABLE"),
                     log_table = Sys.getenv("LOG_TABLE")
                   ),
                   db_update = NULL,
                   db_logic = NULL,
                   cookie_expiration_days = 0,
                   custom_css = NULL,
                   suppress_logs = FALSE) {
  if (missing(json) && missing(list)) {
    stop("Survey JSON or list is required")
  }

  # Check if list contains multiple surveys (multisurvey mode)
  is_multisurvey <- !missing(list) && missing(json) &&
    is.list(list) &&
    length(list) > 0 &&
    !is.null(names(list)) &&
    all(sapply(list, function(x) is.list(x) && ("title" %in% names(x) || "pages" %in% names(x))))

  if (!missing(list) & missing(json) && !is_multisurvey) {
    json <- jsonlite::toJSON(list,
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }

  survey_setup(db_config, shiny_config, is_multisurvey)

  ui <- if (is_multisurvey) {
    # Multisurvey UI with landing page
    fluidPage(
      shinyjs::useShinyjs(),
      # Landing page
      shiny::div(
        id = "landingPage",
        style = "text-align: center; padding: 50px;",
        shiny::h1("Survey Portal"),
        shiny::div(
          id = "surveyLinks",
          lapply(names(list), function(survey_name) {
            shiny::div(
              style = "margin: 10px; padding: 20px; border: 1px solid #ccc; border-radius: 5px; display: inline-block; min-width: 200px;",
              shiny::h4(list[[survey_name]]$title %||% survey_name),
              shiny::tags$a(
                href = paste0("?survey=", survey_name),
                "Start",
                class = "btn btn-primary",
                style = paste0("background-color: ", theme_color, "; border-color: ", theme_color, ";")
              )
            )
          })
        )
      ),
      # Survey container (hidden initially)
      shiny::div(
        id = "surveySection",
        style = "display: none;",
        survey_ui_wrapper(
          id = "surveyContainer",
          theme = theme,
          theme_color = theme_color,
          cookie_expiration_days = cookie_expiration_days,
          custom_css = custom_css
        )
      )
    )
  } else {
    # Single survey UI
    fluidPage(
      survey_ui_wrapper(
        id = "surveyContainer",
        theme = theme,
        theme_color = theme_color,
        cookie_expiration_days = cookie_expiration_days,
        custom_css = custom_css
      )
    )
  }

  server <- function(input, output, session) {
    if (!is_multisurvey) {
      hide_and_show("surveyContainer", "waitingMessage")
    }

    rv <- shiny::reactiveValues(
      survey_responses = NULL,
      loading = FALSE,
      survey_completed = FALSE,
      error_message = NULL,
      load_start_time = Sys.time(),
      start_time = NULL,
      complete_time = NULL,
      save_time = NULL,
      duration_load = NULL,
      duration_complete = NULL,
      duration_save = NULL,
      validated_params = NULL,
      survey_def = NULL,
      selected_survey = NULL,
      survey_json = NULL
    )

    server_setup(
      session = session,
      db_config = db_config,
      app_conn = app_conn,
      survey_logger = survey_logger,
      db_ops = db_ops,
      suppress_logs = suppress_logs,
      is_multisurvey = is_multisurvey
    )

    config_list_reactive <- reactive({
      req(!is.null(db_logic))

      # Determine current survey/table for optimization
      current_survey <- if (is_multisurvey) {
        rv$selected_survey
      } else {
        db_config$write_table
      }

      read_and_cache(
        db_ops = db_ops,
        db_logic = db_logic,
        target_survey = current_survey
      )
    })

    validate_params_reactive <- reactive({
      req(!is.null(db_logic))

      config_list <- config_list_reactive()

      config_validation <- validate_db_logic(
        db_logic = db_logic,
        config_list = config_list,
        survey_logger = logger
      )

      if (!config_validation$valid) {
        msg <- paste(
          "Configuration validation failed:",
          paste(config_validation$errors, collapse = "; ")
        )
        logger$log_message(msg, type = "ERROR", zone = "SURVEY")
        return(list(valid = FALSE, errors = config_validation$errors))
      }

      query_list <- parse_query(session)

      param_validation <- validate_url_parameters(
        db_logic = db_logic,
        config_list = config_list,
        query_list = query_list,
        survey_logger = logger
      )

      if (!param_validation$valid) {
        msg <- paste(
          "URL parameter validation failed:",
          paste(param_validation$errors, collapse = "; ")
        )
        logger$log_message(msg, type = "ERROR", zone = "SURVEY")
        return(list(valid = FALSE, errors = param_validation$errors))
        hide_and_show("waitingMessage", "invalidQueryMessage")
      }

      return(list(
        valid = TRUE,
        values = param_validation$values
      ))
    })

    observe({
      req(session)

      tryCatch(
        {
          isolate({
            # Handle multisurvey mode
            if (is_multisurvey) {
              query_list <- parse_query(session)
              survey_param <- query_list$survey

              if (is.null(survey_param) || survey_param == "") {
                # Show landing page
                shinyjs::show("landingPage")
                shinyjs::hide("surveySection")
                return()
              } else if (!survey_param %in% names(list)) {
                # Invalid survey name
                hide_and_show("landingPage", "surveyNotFoundMessage")
                return()
              } else {
                # Valid survey selected
                rv$selected_survey <- survey_param
                rv$survey_json <- jsonlite::toJSON(list[[survey_param]], pretty = TRUE, auto_unbox = TRUE)

                # Update logger to use actual survey name and start logging
                logger$update_survey_name(survey_param)
                logger$log_message("Started session", zone = "SURVEY")

                shinyjs::hide("landingPage")
                shinyjs::show("surveySection")
                hide_and_show("surveyContainer", "waitingMessage")
              }
            } else {
              # Single survey mode - set survey_json
              rv$survey_json <- json
            }

            if (!is.null(db_logic)) {
              validation_result <- validate_params_reactive()

              if (!validation_result$valid) {
                hide_and_show("waitingMessage", "invalidQueryMessage")
                return()
              }

              rv$validated_params <- validation_result$values
            }

            survey_obj <- if (is.character(rv$survey_json)) {
              jsonlite::fromJSON(rv$survey_json, simplifyVector = FALSE)
            } else {
              rv$survey_json
            }

            rv$survey_def <- survey_obj

            validated_params <- transform_validated_params(
              rv$validated_params, config_list_reactive()
            )

            # Check if current survey has relevant database logic configs
            has_relevant_configs <- FALSE
            if (!is.null(db_logic)) {
              current_table <- if (is_multisurvey) rv$selected_survey else db_config$write_table
              relevant_configs <- Filter(function(config) {
                target_tbl <- config$target_tbl
                # If no target_tbl specified, include the config (legacy behavior)
                # If target_tbl matches current survey, include it
                is.null(target_tbl) || target_tbl == current_table
              }, db_logic)
              has_relevant_configs <- length(relevant_configs) > 0
            }

            survey_data <- list(
              survey = survey_obj,
              params = validated_params,
              db_logic = has_relevant_configs
            )

            json <- jsonlite::toJSON(rv$validated_params)

            session$sendCustomMessage("loadSurvey", survey_data)

            # Keep loading spinner visible until database logic config is complete
            observeEvent(input$surveyReady,
              {
                req(session)
                req(db_logic)
                req(db_ops)

                configure_db_logic(
                  db_logic = db_logic,
                  config_list_reactive = config_list_reactive(),
                  session = session,
                  logger = logger,
                  write_table = if (is_multisurvey) rv$selected_survey else db_config$write_table,
                  db_ops = db_ops,
                  db_update = db_update
                )
              },
              once = TRUE
            )

            # Only hide loading spinner after database logic config is complete
            observeEvent(input$dbLogicConfigComplete,
              {
                hide_and_show("waitingMessage", "surveyContainer")

                rv$start_time <- Sys.time()
                rv$duration_load <- as.numeric(
                  difftime(rv$start_time, rv$load_start_time, units = "secs")
                )

                logger$log_message("Loaded survey",
                  zone = "SURVEY"
                )
                
                # Mark survey as loaded to enable error logging
                logger$mark_survey_loaded()
              },
              once = TRUE
            )
          })
        },
        error = function(e) {
          msg <- sprintf("Survey initialization error: %s", e$message)
          logger$log_message(msg, type = "ERROR", zone = "SURVEY")
          shinyjs::show("surveyNotDefinedMessage")
          shinyjs::hide("waitingMessage")
        }
      )
    }) |> bindEvent(session$clientData$url_search, ignoreInit = FALSE)

    observeEvent(input$surveyComplete, {
      shinyjs::hide(
        id = "surveyContainer",
        anim = TRUE,
        animType = "fade"
      )

      rv$survey_completed <- TRUE
      rv$loading <- TRUE
      rv$complete_time <- Sys.time()
      rv$duration_complete <- as.numeric(difftime(
        rv$complete_time,
        rv$start_time,
        units = "secs"
      ))
      logger$log_message("Completed survey", zone = "SURVEY")
    })

    observeEvent(input$surveyData, {
      parsed_data <- tryCatch(
        {
          if (is.character(input$surveyData)) {
            jsonlite::fromJSON(input$surveyData)
          } else {
            input$surveyData
          }
        },
        error = function(e) {
          rv$error_message <- sprintf("Error parsing survey data: %s", e$message)
          logger$log_message(rv$error_message, type = "ERROR", zone = "SURVEY")
          return(NULL)
        }
      )

      if (!is.null(parsed_data)) {
        if (!is.data.frame(parsed_data) && !is.list(parsed_data)) {
          rv$error_message <- "Invalid data format: expected data frame or list"
          logger$log_message(rv$error_message, type = "ERROR", zone = "SURVEY")
          hide_and_show("savingDataMessage", "invalidDataMessage")
          return(NULL)
        }

        if (is.list(parsed_data) && !is.data.frame(parsed_data)) {
          # Log the structure of the data before conversion
          logger$log_message(
            sprintf(
              "Received data from SurveyJS: %s",
              jsonlite::toJSON(parsed_data, auto_unbox = TRUE)
            ),
            "INFO",
            "SURVEY"
          )

          # Ensure _other fields exist for all fields with showOtherItem = TRUE
          survey_def <- isolate(rv$survey_def)
          if (!is.null(survey_def) && !is.null(db_ops)) {
            # Get all field names that have showOtherItem enabled
            # We need to create a temporary helper function since we can't access private methods
            get_fields_with_other_option <- function(survey_obj) {
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
            }
            
            other_fields <- get_fields_with_other_option(survey_def)
            
            for (field_name in other_fields) {
              other_field_name <- paste0(field_name, "_other")
              
              if (field_name %in% names(parsed_data)) {
                field_value <- parsed_data[[field_name]]
                
                # Check if the main field contains non-numeric text (indicating "other" was selected)
                if (!is.na(field_value) && is.character(field_value) && 
                    !grepl("^\\d+$", field_value)) {
                  # Move the text to the _other column
                  parsed_data[[other_field_name]] <- field_value
                  # Set main field to NULL (will be stored as NULL in database)
                  parsed_data[[field_name]] <- NA
                  
                  logger$log_message(
                    sprintf("Moved 'other' text from '%s' to '%s': %s", 
                           field_name, other_field_name, field_value),
                    "INFO",
                    "SURVEY"
                  )
                } else {
                  # Regular choice selected, ensure _other field exists but is empty
                  if (!other_field_name %in% names(parsed_data)) {
                    parsed_data[[other_field_name]] <- NA_character_
                  }
                }
              }
            }
          }

          # Convert to data frame while preserving all fields including field-other
          parsed_data <- as.data.frame(parsed_data, stringsAsFactors = FALSE)
        }

        tryCatch(
          {
            if (is.null(db_ops)) {
              msg <- "Database operations not initialized"
              logger$log_message(msg, type = "ERROR", zone = "DATABASE")
              stop(msg)
            }

            # Remove tracking columns - these will go in the log table instead

            # Log the final data structure being sent to database
            logger$log_message(
              sprintf(
                "Processed final data: %s",
                jsonlite::toJSON(parsed_data, auto_unbox = TRUE)
              ),
              "INFO",
              "SURVEY"
            )

            # Determine table name (use survey name directly for multisurvey mode)
            table_name <- if (is_multisurvey && !is.null(rv$selected_survey)) {
              rv$selected_survey
            } else {
              db_config$write_table
            }

            # Check if this is a db_update operation
            is_update_operation <- FALSE
            update_config <- NULL
            
            if (!is.null(db_update) && is_multisurvey && !is.null(rv$selected_survey)) {
              # Find matching update configuration
              for (config in db_update) {
                if (!is.null(config$from) && config$from == rv$selected_survey) {
                  is_update_operation <- TRUE
                  update_config <- config
                  break
                }
              }
            }

            if (is_update_operation) {
              # Handle update operation
              target_table <- update_config$to
              join_columns <- update_config$by
              
              # Remove join column from data for table creation (we don't want to add it as a new column)
              join_col_name <- names(join_columns)[1]  # e.g., "grant_drops_id"
              schema_data <- parsed_data[!names(parsed_data) %in% join_col_name]
              
              # First create/ensure the target table exists with the correct schema (excluding join column)
              if (ncol(schema_data) > 0) {
                db_ops$create_survey_table(
                  target_table,
                  schema_data,
                  survey_obj = isolate(rv$survey_def)
                )
              }
              
              # Perform update instead of insert
              result <- db_ops$perform_survey_update(
                source_data = parsed_data,
                target_table = target_table,
                join_columns = join_columns
              )
            } else {
              # Regular insert operation
              # First create/ensure the table exists with the correct schema
              db_ops$create_survey_table(
                table_name,
                parsed_data,
                survey_obj = isolate(rv$survey_def)
              )

              # Then insert the survey data
              result <- db_ops$update_survey_table(table_name, parsed_data)
            }
            
            # Get the survey ID for logging
            survey_id <- tryCatch({
              if (is_update_operation) {
                # For updates, get the ID from the target table using the join value
                join_value <- parsed_data[[names(update_config$by)[1]]]
                target_row <- db_ops$read_table(
                  update_config$to,
                  columns = "id",
                  filters = setNames(list(join_value), update_config$by[1]),
                  limit = 1,
                  update_last_sql = FALSE
                )
                if (nrow(target_row) > 0) target_row$id else NA
              } else {
                # For inserts, get the most recent insert for this survey
                recent_row <- db_ops$read_table(
                  table_name, 
                  columns = "id", 
                  order_by = "id", 
                  desc = TRUE, 
                  limit = 1,
                  update_last_sql = FALSE
                )
                if (nrow(recent_row) > 0) recent_row$id else NA
              }
            }, error = function(e) NA)
            
            # Get client IP address
            client_ip <- session$clientData$url_hostname %||% "unknown"
            
            # Log the successful submission with timing data
            logger$log_entry(
              survey_id = survey_id,
              ip_address = client_ip,
              duration_load = rv$duration_load,
              duration_complete = rv$duration_complete,
              duration_save = as.numeric(difftime(Sys.time(), rv$complete_time, units = "secs"))
            )

            rv$save_time <- Sys.time()
            rv$duration_save <- as.numeric(
              difftime(rv$save_time, rv$complete_time, units = "secs")
            )

            # Tracking fields removed - timing data now in log table

            rv$survey_responses <- parsed_data
            rv$error_message <- NULL

            shinyjs::show(
              id = "surveyContainer",
              anim = TRUE,
              animType = "fade"
            )

            if (show_response) {
              hide_and_show("savingDataMessage", "surveyResponseContainer")
            } else {
              shinyjs::hide(id = "savingDataMessage")
            }

            # Duration tracking removed - timing data now logged per survey submission
          },
          error = function(e) {
            rv$error_message <- sprintf("Database error: %s", e$message)
            # Error already logged by db_ops with SQL statement
            logger$log_message(rv$error_message, type = "ERROR", zone = "DATABASE")
            hide_and_show("savingDataMessage", "invalidDataMessage")
          }
        )
      }

      rv$loading <- FALSE
    })

    server_response(
      output,
      rv,
      show_response = show_response,
      theme_color = theme_color
    )

    server_clean(session, logger)
  }

  shiny::shinyApp(ui, server)
}
