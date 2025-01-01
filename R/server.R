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
#'     workers = 100,
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
#' @importFrom shinyjs show
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
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
                            write_table = Sys.getenv("WRITE_TABLE")
                          )) {

  # Input validation
  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  if (!is.character(db_config$write_table) || nchar(db_config$write_table) == 0) {
    stop("db_config$write_table must be a non-empty character string")
  }

  # Apply Shiny configuration if provided
  if (!is.null(shiny_config)) {
    do.call(configure_shiny, shiny_config)
  }

  # Initialize global database pool if not exists
  if (!exists("app_pool", envir = .GlobalEnv)) {
    assign("app_pool", do.call(initialize_pool, db_config), envir = .GlobalEnv)
  }

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
          shiny::h3("Survey Responses"),
          DT::DTOutput("surveyResponseTable")
        )
      )
    )
  }

  # Define server
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      survey_responses = NULL
    )

    # Initialize database operations
    db_ops <- db_ops$new(get("app_pool", envir = .GlobalEnv), session$token)

    # Load survey
    shiny::observe({
      tryCatch({
        survey_obj <- if (is.character(json)) {
          jsonlite::fromJSON(json, simplifyVector = FALSE)
        } else {
          json
        }
        session$sendCustomMessage("loadSurvey", survey_obj)
      }, error = function(e) {
        warning("Error loading survey JSON: ", e$message)
      })
    })

    # Handle survey responses
    shiny::observeEvent(input$surveyData, {
      parsed_data <- tryCatch({
        if (is.character(input$surveyData)) {
          jsonlite::fromJSON(input$surveyData)
        } else {
          input$surveyData
        }
      }, error = function(e) {
        warning("Error parsing survey data: ", e$message)
        return(NULL)
      })

      if (!is.null(parsed_data)) {
        # Convert to data frame if necessary
        if (!is.data.frame(parsed_data)) {
          parsed_data <- as.data.frame(t(unlist(parsed_data)),
                                       stringsAsFactors = FALSE)
        }
        rv$survey_responses <- parsed_data

        # Store in database
        tryCatch({
          if (!db_ops$check_table_exists(db_config$write_table)) {
            db_ops$create_survey_data_table(db_config$write_table, parsed_data)
            db_ops$update_survey_data_table(db_config$write_table, parsed_data)
          } else {
            db_ops$update_survey_data_table(db_config$write_table, parsed_data)
          }
        }, error = function(e) {
          warning("Error writing to database: ", e$message)
        })

        # Show response table if enabled
        if (show_response) {
          shinyjs::show("surveyResponseContainer")
        }
      }
    })

    # Render response table
    output$surveyResponseTable <- DT::renderDT({
      shiny::req(rv$survey_responses)
      DT::datatable(
        rv$survey_responses,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
      )
    })

    # Control response table visibility
    output$showResponseTable <- shiny::reactive(show_response)
    shiny::outputOptions(output, "showResponseTable", suspendWhenHidden = FALSE)
  }

  # Create and return Shiny app
  shiny::shinyApp(ui, server)
}
