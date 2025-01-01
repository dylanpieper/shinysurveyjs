#' Create a single survey application
#'
#' Creates a Shiny application for a single survey with database integration.
#' Responses are stored in a PostgreSQL database with configurable table name.
#'
#' @param json Survey JSON string or object defining the survey structure
#' @param table_name Name of the database table to store survey responses (default: "survey_responses")
#' @param show_response Logical. Whether to show responses after submission (default: TRUE)
#' @param theme Theme name for SurveyJS: "defaultV2" or "modern" (default: "defaultV2")
#' @param theme_color Primary color hex code for theme customization
#' @param theme_mode Color mode selection: "light" or "dark" (default: "light")
#' @param shiny_config Optional list. Shiny configuration parameters passed to configure_shiny()
#' @param db_config List. Database connection parameters (defaults to environment variables):
#'   \itemize{
#'     \item host: Database host (default: HOST environment variable)
#'     \item port: Database port (default: PORT environment variable)
#'     \item dbname: Database name (default: DBNAME environment variable)
#'     \item user: Database username (default: USER environment variable)
#'     \item password: Database password (default: PASSWORD environment variable)
#'   }
#'
#' @return A Shiny application object
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' survey <- survey_single(json_string)
#'
#' # Custom table name and theme
#' feedback_survey <- survey_single(
#'   json = feedback_json,
#'   table_name = "customer_feedback",
#'   theme = "modern",
#'   theme_color = "#4CAF50"
#' )
#'
#' # Multiple surveys with different configurations
#' employee_survey <- survey_single(
#'   json = employee_json,
#'   table_name = "employee_satisfaction",
#'   show_response = FALSE
#' )
#' }
#'
#' @importFrom shiny fluidPage observeEvent reactive reactiveValues req outputOptions shinyApp
#' @importFrom shinyjs show
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
#' @export
survey_single <- function(json,
                          table_name = "survey_responses",
                          show_response = TRUE,
                          theme = "defaultV2",
                          theme_color = "#003594",
                          theme_mode = "light",
                          shiny_config = NULL,
                          db_config = list(
                            host = Sys.getenv("HOST"),
                            port = as.numeric(Sys.getenv("PORT")),
                            dbname = Sys.getenv("DBNAME"),
                            user = Sys.getenv("USER"),
                            password = Sys.getenv("PASSWORD")
                          )) {

  # Input validation
  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  if (!is.character(table_name) || nchar(table_name) == 0) {
    stop("table_name must be a non-empty character string")
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
          if (!db_ops$check_table_exists(table_name)) {
            db_ops$create_survey_data_table(table_name, parsed_data)
            db_ops$update_survey_data_table(table_name, parsed_data)
          } else {
            db_ops$update_survey_data_table(table_name, parsed_data)
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
