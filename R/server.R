#' Create a single survey
#'
#' @param json Survey JSON string or object
#' @param show_responses Whether to show responses after submission
#' @param theme Theme name ("defaultV2" or "modern")
#' @param primary Primary color hex colde (optional)
#'
#' @import shiny
#' @import shinyjs
#' @importFrom DT renderDT datatable
#' @import jsonlite
#' @import sass
#' @export
survey_single <- function(json,
                          show_responses = TRUE,
                          theme = "defaultV2",
                          theme_primary = "#003594",
                          theme_mode = "light") {
  # Validate inputs
  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  # Create UI function
  ui <- function() {
    fluidPage(
      useShinyjs(),
      surveyUI(theme = theme, primary = theme_primary, mode = theme_mode)
    )
  }

  # Create server function
  server <- function(input, output, session) {
    # Initialize reactive values
    rv <- reactiveValues(
      survey_responses = NULL
    )

    # Load survey on start
    observe({
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

    # Process survey completion
    observeEvent(input$surveyData, {
      # Parse survey data
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
        # Convert to data frame if needed
        if (!is.data.frame(parsed_data)) {
          parsed_data <- as.data.frame(t(unlist(parsed_data)),
                                       stringsAsFactors = FALSE)
        }
        # Store processed data
        rv$survey_responses <- parsed_data

        # Show results container if enabled
        if (show_responses) {
          shinyjs::show("surveyResponseContainer")
        }
      }
    })

    # Render response table
    output$surveyResponseTable <- DT::renderDT({
      req(rv$survey_responses)
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

    # Control visibility of responses
    output$showResponseTable <- reactive(show_responses)
    outputOptions(output, "showResponseTable", suspendWhenHidden = FALSE)
  }

  # Return Shiny app
  shinyApp(ui, server)
}
