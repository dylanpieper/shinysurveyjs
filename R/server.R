#' Create a single survey
#'
#' @param json Survey JSON string or object
#' @param show_responses Whether to show responses after submission
#' @param theme Theme name ("defaultV2" or "modern")
#' @param theme_color Primary color hex code for theme (optional)
#' @param theme_mode Color mode selection: "light" or "dark" (default: "light")
#' @param shiny_config Optional Shiny configuration parameters passed to configure_shiny()
#'
#' @importFrom shiny fluidPage observeEvent reactive reactiveValues req outputOptions shinyApp
#' @importFrom shinyjs show
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON
#' @export
survey_single <- function(json, show_responses = TRUE, theme = "defaultV2",
                          theme_color = "#003594", theme_mode = "light",
                          shiny_config = NULL) {
  if (!is.null(shiny_config)) {
    do.call(configure_shiny, shiny_config)
  }

  if (missing(json) || is.null(json)) {
    stop("Survey JSON is required")
  }

  ui <- function() {
    shiny::fluidPage(
      surveyUI(theme = theme, primary = theme_color, mode = theme_mode)
    )
  }

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      survey_responses = NULL
    )

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
        if (!is.data.frame(parsed_data)) {
          parsed_data <- as.data.frame(t(unlist(parsed_data)),
                                       stringsAsFactors = FALSE)
        }
        rv$survey_responses <- parsed_data
        if (show_responses) {
          shinyjs::show("surveyResponseContainer")
        }
      }
    })

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

    output$showResponseTable <- shiny::reactive(show_responses)
    shiny::outputOptions(output, "showResponseTable", suspendWhenHidden = FALSE)
  }

  shiny::shinyApp(ui, server)
}
