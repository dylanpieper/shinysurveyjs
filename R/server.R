#' Create Single Survey
#'
#' Creates a Shiny application for a single survey with database integration.
#' Responses are stored in a PostgreSQL database with configurable table name.
#'
#' @param json Survey JSON string or object defining the survey structure
#' @param show_response Logical. Whether to show responses after submission (default: TRUE)
#' @param theme Theme name for SurveyJS: "defaultV2" or "modern" (default: "defaultV2")
#' @param theme_color Primary color hex code for theme customization
#' @param theme_mode Color mode selection: "light" or "dark" (default: "light")
#' @param shiny_config Optional list. Shiny configuration parameters passed to configure_shiny()
#' @param db_config List. Database connection parameters (defaults to environment variables):
#'   \itemize{
#'     \item **host**: Database host (default: HOST environment variable)
#'     \item **port**: Database port (default: PORT environment variable)
#'     \item **db_name**: Database name (default: DB_NAME environment variable)
#'     \item **user**: Database username (default: USER environment variable)
#'     \item **password**: Database password (default: PASSWORD environment variable)
#'     \item **write_table**: Table name to write survey data (default: WRITE_TABLE environment variable)
#'     \item **log_table**: Table name to write log messages (default: LOG_TABLE environment variable)
#'   }
#' @param cookie_expiration_days Number of days to keep cookies for URL parameters (default: 7)
#'
#' @return A Shiny application object
#'
#' @importFrom shiny fluidPage observeEvent reactive reactiveValues req outputOptions shinyApp
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
#' @importFrom shinyjs hide show
#' @importFrom future plan multisession
#'
#' @export
survey_single <- function(json,
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
                          suppress_logs = FALSE) {
  # Validate survey JSON
  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  # Setup survey
  survey_setup(db_config, shiny_config)

  # Define UI
  ui <- fluidPage(
    survey_ui_wrapper(
      id = "surveyContainer",
      theme = theme,
      theme_color = theme_color,
      theme_mode = theme_mode,
      cookie_expiration_days = cookie_expiration_days
    )
  )

  # Define server
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
      duration_load = NULL,
      duration_complete = NULL,
      validated_params = NULL
    )

    # Setup survey app logger and database pool
    server_setup(
      session = session,
      db_config = db_config,
      app_pool = app_pool,
      survey_logger = survey_logger,
      db_ops = db_ops,
      suppress_logs = suppress_logs
    )

    # Create reactive expressions for cached config and validation
    config_list_reactive <- reactive({
      req(!is.null(dynamic_config))
      read_and_cache(
        db_ops = db_ops,
        dynamic_config = dynamic_config
      )
    })

    validate_params_reactive <- reactive({
      req(!is.null(dynamic_config))

      # Get cached config
      config_list <- config_list_reactive()

      # Validate configuration
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

      # Parse URL parameters
      query_list <- parse_query(session)

      # Validate parameters
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

    # Load survey with error handling
    observe({
      req(session)

      tryCatch({
        isolate({
          if (!is.null(dynamic_config)) {
            # Get validated parameters
            validation_result <- validate_params_reactive()

            if (!validation_result$valid) {
              hide_and_show("waitingMessage", "invalidQueryMessage")
              return()
            }

            # Store validated params in reactive values
            rv$validated_params <- validation_result$values
          }

          # Parse survey JSON if needed
          survey_obj <- if (is.character(json)) {
            jsonlite::fromJSON(json, simplifyVector = FALSE)
          } else {
            json
          }

          # Construct the data to send to JavaScript with validated parameters
          validated_params <- transform_validated_params(rv$validated_params, config_list_reactive())

          # Log the parameters we're about to send
          logger$log_message(
            sprintf(
              "Attaching JSON to survey data: %s",
              jsonlite::toJSON(rv$validated_params)
            ),
            zone = "SURVEY"
          )

          survey_data <- list(
            survey = survey_obj,
            params = validated_params
          )

          # Send survey and parameters to client
          session$sendCustomMessage("loadSurvey", survey_data)

          # print("dynamic_config")
          # print(dynamic_config)
          # print("config_list_reactive")
          # print(config_list_reactive())
          # print("validated_params")
          # print(validated_params)

          # Configure dynamic fields
          configure_dynamic_fields(
            dynamic_config = dynamic_config,
            config_list_reactive = config_list_reactive(),
            session = session,
            logger = logger
            )

          hide_and_show("waitingMessage", "surveyContainer")

          # Update timing information
          rv$start_time <- Sys.time()
          rv$duration_load <- as.numeric(
            difftime(rv$start_time, rv$load_start_time, units = "secs")
          )

          # Log successful initialization
          logger$log_message("Loaded survey", zone = "SURVEY")
        })
      },
      error = function(e) {
        msg <- sprintf("Survey initialization error: %s", e$message)
        logger$log_message(msg, type = "ERROR", zone = "SURVEY")
        shinyjs::show("surveyNotDefinedMessage")
        shinyjs::hide("waitingMessage")
      })
    }) |> bindEvent(session$clientData$url_search, ignoreInit = FALSE)

    # Handle survey completion
    observeEvent(input$surveyComplete, {
      rv$survey_completed <- TRUE
      rv$loading <- TRUE
      rv$complete_time <- Sys.time()
      rv$duration_complete <- as.numeric(difftime(rv$complete_time, rv$start_time, units = "secs"))

      # Log successful completion
      logger$log_message("Completed survey", zone = "SURVEY")
    })

    # Handle survey responses with timing data
    observeEvent(input$surveyData, {
      # Parse survey data
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
        # Validate parsed data
        if (!is.data.frame(parsed_data) && !is.list(parsed_data)) {
          rv$error_message <- "Invalid data format: expected data frame or list"
          logger$log_message(rv$error_message, type = "ERROR", zone = "SURVEY")
          hide_and_show("savingDataMessage", "invalidDataMessage")
          return(NULL)
        }

        # Convert list to data frame if necessary
        if (is.list(parsed_data) && !is.data.frame(parsed_data)) {
          parsed_data <- as.data.frame(parsed_data, stringsAsFactors = FALSE)
        }

        # Add only validated parameters to the data frame
        if (!is.null(rv$validated_params) && length(rv$validated_params) > 0) {
          for (param_name in names(rv$validated_params)) {
            parsed_data[[param_name]] <- rv$validated_params[[param_name]]
          }
        }

        # Store in database with error handling
        tryCatch(
          {
            if (is.null(db_ops)) {
              msg <- "Database operations not initialized"
              logger$log_message(msg, type = "ERROR", zone = "DATABASE")
              stop(msg)
            }

            # Add timing data
            parsed_data$duration_load <- rv$duration_load
            parsed_data$duration_complete <- rv$duration_complete

            # Create/update table schema if needed
            db_ops$create_survey_table(db_config$write_table, parsed_data)

            # Insert the data
            db_ops$update_survey_table(db_config$write_table, parsed_data)

            # Update reactive value after successful database operation
            rv$survey_responses <- parsed_data
            rv$error_message <- NULL

            hide_and_show("savingDataMessage", "surveyResponseContainer")
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

    # Render response table
    server_response(output, rv, show_response = show_response)

    # Clean up on session end
    server_clean(session, logger)
  }

  # Start Shiny app
  shiny::shinyApp(ui, server)
}
