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
#'     \item write_table: Table name to write log messages (default: LOG_TABLE environment variable)
#'   }
#'
#' @return A Shiny application object
#'
#' @examples
#' \dontrun{
#' library(shinysurveyjs)
#'
#' # Define a package feedback survey with rating and conditional comment
#' survey <- '{
#'   "title": "R Package Feedback",
#'   "pages": [
#'     {
#'       "name": "userInfo",
#'       "elements": [
#'         {
#'           "type": "rating",
#'           "name": "rating",
#'           "title": "Please rate the shinysurveyjs 📦:",
#'           "rateValues": [
#'             {"value": 1, "text": "⭐"},
#'             {"value": 2, "text": "⭐⭐"},
#'             {"value": 3, "text": "⭐⭐⭐"},
#'             {"value": 4, "text": "⭐⭐⭐⭐"},
#'             {"value": 5, "text": "⭐⭐⭐⭐⭐"}
#'           ],
#'           "rateMax": 5
#'         },
#'         {
#'           "type": "comment",
#'           "name": "feedback",
#'           "visibleIf": "{rating} notempty",
#'           "title": "Why did you rate it {rating} stars?",
#'           "rows": 2
#'         }
#'       ]
#'     }
#'   ]
#' }'
#'
#' # Create and launch the survey application
#' survey_single(
#'   json = survey,
#'   show_response = TRUE,
#'   theme_color = "#00AD6E",
#'   theme_mode = "dark",
#'   shiny_config = list(
#'     host = "0.0.0.0",
#'     port = 3838,
#'     sanitize_errors = TRUE,
#'     autoreload = FALSE
#'   ),
#'   db_config = list(
#'     host = "pooler.supabase.com",
#'     port = 5432,
#'     dbname = "postgres",
#'     user = Sys.getenv("DB_USER"),
#'     password = Sys.getenv("DB_PASSWORD"),
#'     write_table = "survey_package_feedback"
#'   )
#' )
#' }
#'
#' @importFrom shiny fluidPage observeEvent reactive reactiveValues req outputOptions shinyApp
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
#' @importFrom future plan multisession
#'
#' @param json Survey JSON string or object
#' @param show_response Logical to show responses
#' @param theme Theme name
#' @param theme_color Primary color hex
#' @param theme_mode Color mode
#' @param shiny_config Shiny configuration
#' @param db_config Database configuration
#' @return Shiny application object
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
  # Validate inputs with detailed error messages
  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  if (!is.character(db_config$write_table) || nchar(db_config$write_table) == 0) {
    stop("db_config$write_table must be a non-empty character string")
  }

  # Validate database configuration
  required_db_fields <- c("host", "port", "db_name", "user", "password")
  missing_fields <- required_db_fields[!required_db_fields %in% names(db_config)]
  if (length(missing_fields) > 0) {
    stop(sprintf(
      "Missing required database configuration fields: %s",
      paste(missing_fields, collapse = ", ")
    ))
  }

  # Set database environment variables
  Sys.setenv(
    "HOST" = db_config$host,
    "PORT" = as.character(db_config$port),
    "DB_NAME" = db_config$db_name,
    "USER" = db_config$user,
    "PASSWORD" = db_config$password
  )

  # Apply Shiny configuration if provided
  if (!is.null(shiny_config)) {
    do.call(configure_shiny, shiny_config)
  }

  # Initialize global database pool with error handling
  if (!exists("app_pool", envir = .GlobalEnv)) {
    tryCatch(
      {
        assign("app_pool", do.call(
          initialize_pool,
          db_config[c("host", "port", "db_name", "user", "password")]
        ),
        envir = .GlobalEnv
        )
      },
      error = function(e) {
        stop(sprintf("Failed to initialize database pool: %s", e$message))
      }
    )
  }

  # Initialize future plan for async operations
  future::plan(future::multisession)

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
      survey_name = db_config$write_table,
      db_config = db_config[c("host", "port", "db_name", "user", "password")]
    )

    logger$log_message("Starting survey")

    rv <- shiny::reactiveValues(
      survey_responses = NULL,
      loading = FALSE,
      survey_completed = FALSE,
      error_message = NULL
    )

    # Initialize database operations with error logging
    db_ops <- tryCatch(
      {
        db_ops$new(get("app_pool", envir = .GlobalEnv), session$token)
      },
      error = function(e) {
        warning(sprintf("Failed to initialize db_ops: %s", e$message))
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
        },
        error = function(e) {
          rv$error_message <- sprintf("Error loading survey: %s", e$message)
          warning(rv$error_message)
          shinyjs::show("surveyNotDefinedMessage")
        }
      )
    })

    # Handle survey completion
    observeEvent(input$surveyComplete, {
      shinyjs::show("savingDataMessage")
      rv$survey_completed <- TRUE
      rv$loading <- TRUE
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
          warning(rv$error_message)
          return(NULL)
        }
      )

      if (!is.null(parsed_data)) {
        # Validate parsed data
        if (!is.data.frame(parsed_data) && !is.list(parsed_data)) {
          rv$error_message <- "Invalid data format: expected data frame or list"
          warning(rv$error_message)
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
              stop("Database operations not initialized")
            }

            if (!db_ops$check_table_exists(db_config$write_table)) {
              db_ops$create_survey_data_table(db_config$write_table, parsed_data)
            }

            db_ops$update_survey_data_table(db_config$write_table, parsed_data)

            # Update reactive value after successful database operation
            rv$survey_responses <- parsed_data
            rv$error_message <- NULL

            # Hide saving spinner and show response table
            hide_and_show_message("savingDataMessage", "surveyResponseContainer")
          },
          error = function(e) {
            rv$error_message <- sprintf("Database error: %s", e$message)
            warning(rv$error_message)
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
      cleanup_app(session)
    })
  }

  # Create and return Shiny app
  shiny::shinyApp(ui, server)
}
