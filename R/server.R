#' Create a Single Survey
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
#'     \item host: Database host (default: HOST environment variable)
#'     \item port: Database port (default: PORT environment variable)
#'     \item db_name: Database name (default: DB_NAME environment variable)
#'     \item user: Database username (default: USER environment variable)
#'     \item password: Database password (default: PASSWORD environment variable)
#'     \item write_table: Table name to write survey data (default: WRITE_TABLE environment variable)
#'     \item log_table: Table name to write log messages (default: LOG_TABLE environment variable)
#'   }
#'
#' @return A Shiny application object
#'
#' @importFrom shiny fluidPage observeEvent reactive reactiveValues req outputOptions shinyApp
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
#' @importFrom future plan multisession
#'
#' @export
survey_single <- function(json,
                          show_response = TRUE,
                          theme = "defaultV2",
                          theme_color = "#003594",
                          theme_mode = "light",
                          shiny_config = NULL,
                          db_config = list(
                            host = Sys.getenv("HOST"),
                            port = as.numeric(Sys.getenv("PORT")),
                            db_name = Sys.getenv("DB_NAME"),
                            user = Sys.getenv("USER"),
                            password = Sys.getenv("PASSWORD"),
                            write_table = Sys.getenv("WRITE_TABLE"),
                            log_table = Sys.getenv("LOG_TABLE")
                          )) {
  # Validate survey JSON
  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  # Initialize survey
  setup_survey(db_config, shiny_config)

  # Define UI
  ui <- function() {
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      surveyUI(theme = theme, primary = theme_color, mode = theme_mode),
      shiny::conditionalPanel(
        condition = "output.showResponseTable",
        shiny::div(
          id = "surveyResponseContainer",
          style = "display: none;",
          shiny::h3("Your Response:"),
          shiny::div(
            class = "nested-spinner-container",
            shinycssloaders::withSpinner(
              DT::DTOutput("surveyResponseTable"),
              type = 8,
              color = theme_color,
              size = 1,
              proxy.height = "200px"
            )
          )
        )
      )
    )
  }

  # Define server with enhanced error handling
  server <- function(input, output, session) {
    logger <- survey_logger$new(
      log_table = db_config$log_table,
      session_id = session$token,
      survey_name = db_config$write_table
    )

    logger$log_message("Started session", zone = "SURVEY")

    rv <- shiny::reactiveValues(
      survey_responses = NULL,
      loading = FALSE,
      survey_completed = FALSE,
      error_message = NULL
    )

    # Initialize database operations with error logging
    db_ops <- tryCatch(
      {
        db_ops$new(get("app_pool", envir = .GlobalEnv), session$token, logger)
      },
      error = function(e) {
        msg <- sprintf("Failed to initialize db_ops: %s", e$message)
        logger$log_message(msg, type = "ERROR", zone = "DATABASE")
        NULL
      }
    )

    # Load survey with error handling
    shiny::observe({
      tryCatch(
        {
          survey_obj <- if (is.character(json)) {
            jsonlite::fromJSON(json, simplifyVector = FALSE)
          } else {
            json
          }
          session$sendCustomMessage("loadSurvey", survey_obj)
          logger$log_message("Loaded survey", zone = "SURVEY")
        },
        error = function(e) {
          rv$error_message <- sprintf("Error loading survey: %s", e$message)
          logger$log_message(rv$error_message, type = "ERROR", zone = "SURVEY")
          shinyjs::show("surveyNotDefinedMessage")
        }
      )
    })

    # Handle survey completion
    observeEvent(input$surveyComplete, {
      shinyjs::show("savingDataMessage")
      rv$survey_completed <- TRUE
      rv$loading <- TRUE
      logger$log_message("Completed survey", zone = "SURVEY")
    })

    # Handle survey responses with detailed error handling
    observeEvent(input$surveyData, {
      logger$log_message("Processed data", zone = "SURVEY")

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
          hide_and_show_message("savingDataMessage", "invalidDataMessage")
          return(NULL)
        }

        # Convert list to data frame if necessary
        if (is.list(parsed_data) && !is.data.frame(parsed_data)) {
          parsed_data <- as.data.frame(parsed_data, stringsAsFactors = FALSE)
        }

        # Store in database with error handling
        tryCatch(
          {
            if (is.null(db_ops)) {
              msg <- "Database operations not initialized"
              logger$log_message(msg, type = "ERROR", zone = "DATABASE")
              stop(msg)
            }

            db_ops$create_survey_data_table(db_config$write_table, parsed_data)

            db_ops$update_survey_data_table(db_config$write_table, parsed_data)

            # Update reactive value after successful database operation
            rv$survey_responses <- parsed_data
            rv$error_message <- NULL

            # Hide saving spinner and show response table
            hide_and_show_message("savingDataMessage", "surveyResponseContainer")
          },
          error = function(e) {
            rv$error_message <- sprintf("Database error: %s", e$message)
            logger$log_message(rv$error_message, type = "ERROR", zone = "DATABASE")
            hide_and_show_message("savingDataMessage", "invalidDataMessage")
          }
        )
      }

      rv$loading <- FALSE
    })

    # Render response table with error handling
    output$surveyResponseTable <- DT::renderDT({
      shiny::req(rv$survey_completed)
      shiny::validate(need(!rv$loading, "Loading data..."))
      shiny::req(rv$survey_responses)

      if (!is.null(rv$error_message)) {
        return(NULL)
      }

      DT::datatable(
        rv$survey_responses,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "tp"
        ),
        rownames = FALSE
      )
    })

    # Control response table visibility
    output$showResponseTable <- shiny::reactive({
      show_response && rv$survey_completed && is.null(rv$error_message)
    })
    shiny::outputOptions(output, "showResponseTable", suspendWhenHidden = FALSE)

    # Clean up on session end
    session$onSessionEnded(function() {
      logger$log_message("Ended session", zone = "SURVEY")
      db_pool_close(session)
    })
  }

  # Create and return Shiny app
  shiny::shinyApp(ui, server)
}
