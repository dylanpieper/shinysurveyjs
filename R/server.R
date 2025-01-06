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
                          show_response = TRUE,
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
                          cookie_expiration_days = 7) {
  # Validate survey JSON
  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  # Setup survey
  survey_setup(db_config, shiny_config)

  # Define UI
  ui <- fluidPage(
    survey_ui_wrapper(theme = theme, theme_color = theme_color, theme_mode = theme_mode, cookie_expiration_days = cookie_expiration_days)
  )

  # Define server
  server <- function(input, output, session) {

    shinyjs::show("waitingMessage", anim = TRUE, animType = "fade", time = .25)

    # Setup reactive values
    rv <- shiny::reactiveValues(
      survey_responses = NULL,
      loading = FALSE,
      survey_completed = FALSE,
      error_message = NULL,
      start_time = Sys.time(),
      complete_time = NULL,
      duration = NULL
    )

    # Setup survey app logger and database pool
    server_setup(
       session = session,
       db_config = db_config,
       app_pool = app_pool,
       survey_logger = survey_logger,
       db_ops = db_ops
     )

    shinyjs::hide("waitingMessage", anim = TRUE, animType = "fade", time = .25)

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
      rv$complete_time <- Sys.time()
      rv$duration <- difftime(rv$complete_time, rv$start_time, units = "secs")
      logger$log_message("Completed survey", zone = "SURVEY")
    })

    # Handle survey responses with detailed error handling
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

        # Store in database with error handling
        tryCatch(
          {
            if (is.null(db_ops)) {
              msg <- "Database operations not initialized"
              logger$log_message(msg, type = "ERROR", zone = "DATABASE")
              stop(msg)
            }

            parsed_data$duration_complete <- rv$duration

            db_ops$create_survey_table(db_config$write_table, parsed_data)

            db_ops$update_survey_table(db_config$write_table, parsed_data)

            # Update reactive value after successful database operation
            rv$survey_responses <- parsed_data
            rv$error_message <- NULL

            # Hide saving spinner and show response table
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
