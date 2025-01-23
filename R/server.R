#' Deploy a Survey Shiny Application
#'
#' Creates and deploys a Shiny application for a survey using SurveyJS
#' (<https://surveyjs.io>) with PostgreSQL database integration. The application handles
#' survey data collection, dynamic fields, and asynchronous logging through a future plan.
#'
#' @param json String. JSON survey definition or object.
#' @param list List. Survey structure to convert to JSON.
#' @param show_response Logical. Display responses in a data.table after submission.
#'   Default: `FALSE`.
#' @param theme String. SurveyJS theme, either "defaultV2" or "modern".
#'   Default: "defaultV2".
#' @param theme_color String. Hex color code for primary theme customization.
#' @param theme_mode String. Color mode, either "light" or "dark".
#'   Default: "light".
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
#' @param dynamic_config List. Configuration for dynamic fields. Supports three types:
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
#' @param cookie_expiration_days Numeric. Number of days to retain survey cookies.
#'   Default: 7.
#' @param custom_css String. Custom CSS rules to append to the theme.
#' @param suppress_logs Logical. Suppress console log messages. Default: `FALSE`.
#'
#' @return A Shiny application object
#'
#' @examples
#' \dontrun{
#' # Choice configuration example
#' dynamic_config <- list(
#'   list(
#'     config_type = "choice",
#'     table_name = "packages",
#'     config_col = "name"
#'   )
#' )
#'
#' # Parameter configuration example
#' dynamic_config <- list(
#'   list(
#'     config_type = "param",
#'     table_name = "sources",
#'     config_col = "source",
#'     display_col = "display_text"
#'   )
#' )
#'
#' # Unique value configuration example
#' dynamic_config <- list(
#'   list(
#'     config_type = "unique",
#'     config_col = "title",
#'     result = "warn",
#'     result_field = "warning_message"
#'   )
#' )
#' }
#'
#' @importFrom shiny fluidPage observeEvent reactive reactiveValues req outputOptions shinyApp
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom shinyjs hide show
#' @importFrom future plan multisession
#'
#' @export
survey_single <- function(json = NULL,
                          list = NULL,
                          show_response = FALSE,
                          theme = "defaultV2",
                          theme_color = "#003594",
                          theme_mode = "light",
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
                          dynamic_config = NULL,
                          cookie_expiration_days = 7,
                          custom_css = NULL,
                          suppress_logs = FALSE) {
  if (missing(json) && missing(list)) {
    stop("Survey JSON or list is required")
  }

  if (!missing(list) & missing(json)) {
    json <- jsonlite::toJSON(list,
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }

  survey_setup(db_config, shiny_config)

  ui <- fluidPage(
    survey_ui_wrapper(
      id = "surveyContainer",
      theme = theme,
      theme_color = theme_color,
      theme_mode = theme_mode,
      cookie_expiration_days = cookie_expiration_days,
      custom_css = custom_css
    )
  )

  server <- function(input, output, session) {
    hide_and_show("surveyContainer", "waitingMessage")

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
      validated_params = NULL
    )

    server_setup(
      session = session,
      db_config = db_config,
      app_pool = app_pool,
      survey_logger = survey_logger,
      db_ops = db_ops,
      suppress_logs = suppress_logs
    )

    config_list_reactive <- reactive({
      req(!is.null(dynamic_config))
      read_and_cache(
        db_ops = db_ops,
        dynamic_config = dynamic_config
      )
    })

    validate_params_reactive <- reactive({
      req(!is.null(dynamic_config))

      config_list <- config_list_reactive()

      config_validation <- validate_dynamic_config(
        dynamic_config = dynamic_config,
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
        dynamic_config = dynamic_config,
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
            if (!is.null(dynamic_config)) {
              validation_result <- validate_params_reactive()

              if (!validation_result$valid) {
                hide_and_show("waitingMessage", "invalidQueryMessage")
                return()
              }

              rv$validated_params <- validation_result$values
            }

            survey_obj <- if (is.character(json)) {
              jsonlite::fromJSON(json, simplifyVector = FALSE)
            } else {
              json
            }

            validated_params <- transform_validated_params(
              rv$validated_params, config_list_reactive()
            )

            survey_data <- list(
              survey = survey_obj,
              params = validated_params,
              dynamic_config = !is.null(dynamic_config)
            )

            json <- jsonlite::toJSON(rv$validated_params)

            logger$log_message(
              sprintf(
                "Attached JSON to survey data: %s",
                json
              ),
              zone = "SURVEY"
            )

            session$sendCustomMessage("loadSurvey", survey_data)

            # Keep loading spinner visible until dynamic config is complete
            observeEvent(input$surveyReady,
              {
                req(session)
                req(dynamic_config)
                req(db_ops)

                configure_dynamic_fields(
                  dynamic_config = dynamic_config,
                  config_list_reactive = config_list_reactive(),
                  session = session,
                  logger = logger,
                  write_table = db_config$write_table,
                  db_ops = db_ops
                )

                logger$log_message("Configured dynamic fields", zone = "SURVEY")
              },
              once = TRUE
            )

            # Only hide loading spinner after dynamic config is complete
            observeEvent(input$dynamicConfigComplete,
              {
                hide_and_show("waitingMessage", "surveyContainer")

                rv$start_time <- Sys.time()
                rv$duration_load <- as.numeric(
                  difftime(rv$start_time, rv$load_start_time, units = "secs")
                )

                logger$log_message("Loaded survey",
                  zone = "SURVEY"
                )
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
          parsed_data <- as.data.frame(parsed_data, stringsAsFactors = FALSE)
        }

        tryCatch(
          {
            if (is.null(db_ops)) {
              msg <- "Database operations not initialized"
              logger$log_message(msg, type = "ERROR", zone = "DATABASE")
              stop(msg)
            }

            parsed_data$duration_load <- rv$duration_load
            parsed_data$duration_complete <- rv$duration_complete
            parsed_data$duration_save <- as.numeric(0.1)

            # First save the data normally
            db_ops$create_survey_table(db_config$write_table, parsed_data)
            result <- db_ops$update_survey_table(db_config$write_table, parsed_data)

            rv$save_time <- Sys.time()
            rv$duration_save <- as.numeric(
              difftime(rv$save_time, rv$complete_time, units = "secs")
            )

            if (show_response) {
              parsed_data$duration_load <- rv$duration_load |> round(2)
              parsed_data$duration_complete <- rv$duration_complete |> round(2)
              parsed_data$duration_save <- rv$duration_save |> round(2)
              parsed_data$session_id <- session$token
              parsed_data$ip_address <- db_ops$get_client_ip()
              parsed_data$date_created <- Sys.Date()
            }

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

            update_duration_save(
              db_ops = db_ops,
              db_config = db_config,
              session_id = session$token,
              duration_save = rv$duration_save,
              logger = logger
            )
          },
          error = function(e) {
            rv$error_message <- sprintf("Database error: %s", e$message)
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
      theme_mode = theme_mode,
      theme_color = theme_color
    )

    server_clean(session, logger)
  }

  shiny::shinyApp(ui, server)
}
