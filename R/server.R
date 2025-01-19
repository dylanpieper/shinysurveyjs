#' Single Survey Shiny Application
#'
#' Deploys a Shiny application for a single survey with database integration.
#' Survey data and app logs are stored in a PostgreSQL database with configurable table names.
#' App logs are stored asynchronously using a future plan.
#'
#' @param json Survey JSON string or object defining the survey structure
#' @param list List with survey structure to convert to JSON
#' @param show_response Logical. Show responses in a `data.table()` after submission (default: TRUE)
#' @param theme Theme name for SurveyJS: "defaultV2" or "modern" (default: "defaultV2")
#' @param theme_color Primary color hex code for theme customization
#' @param theme_mode Color mode selection: "light" or "dark" (default: "light")
#' @param shiny_config Optional list. Shiny configuration parameters
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
#' @param cookie_expiration_days Number of days to keep cookies for survey data (default: 7)
#' @param suppress_logs Logical. Suppress log messages in the console (default: FALSE)
#'
#' @return A Shiny application object
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
                          suppress_logs = FALSE) {

  if (missing(json) && missing(list)) {
    stop("Survey JSON or list is required")
  }

  if (!missing(list) & missing(json)){
    json <- jsonlite::toJSON(list,
                             pretty = TRUE,
                             auto_unbox = TRUE)
  }

  survey_setup(db_config, shiny_config)

  ui <- fluidPage(
    survey_ui_wrapper(
      id = "surveyContainer",
      theme = theme,
      theme_color = theme_color,
      theme_mode = theme_mode,
      cookie_expiration_days = cookie_expiration_days
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

      tryCatch({
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
            params = validated_params
          )

          logger$log_message(
            sprintf(
              "Attached JSON to survey data: %s",
              jsonlite::toJSON(rv$validated_params)
            ),
            zone = "SURVEY"
          )

          session$sendCustomMessage("loadSurvey", survey_data)

          configure_dynamic_fields(
            dynamic_config = dynamic_config,
            config_list_reactive = config_list_reactive(),
            session = session,
            logger = logger
          )

          hide_and_show("waitingMessage", "surveyContainer")

          rv$start_time <- Sys.time()
          rv$duration_load <- as.numeric(
            difftime(rv$start_time, rv$load_start_time, units = "secs")
          )

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

    observeEvent(input$surveyComplete, {
      shinyjs::hide(id = "surveyContainer",
                    anim = TRUE,
                    animType = "fade")

      rv$survey_completed <- TRUE
      rv$loading <- TRUE
      rv$complete_time <- Sys.time()
      rv$duration_complete <- as.numeric(difftime(
        rv$complete_time,
        rv$start_time,
        units = "secs"))
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

            if(show_response){
              parsed_data$duration_load <- rv$duration_load |> round(2)
              parsed_data$duration_complete <- rv$duration_complete |> round(2)
              parsed_data$duration_save <- rv$duration_save |> round(2)
              parsed_data$session_id <- session$token
              parsed_data$ip_address <- db_ops$get_client_ip()
              parsed_data$date_created <- Sys.Date()
            }

            rv$survey_responses <- parsed_data
            rv$error_message <- NULL

            shinyjs::show(id = "surveyContainer",
                          anim = TRUE,
                          animType = "fade")

            if(show_response){
              hide_and_show("savingDataMessage", "surveyResponseContainer")
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
