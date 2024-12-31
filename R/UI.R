#' Create a surveyjs UI
#'
#' @param id Optional id for the survey div container (default is "surveyContainer")
#' @param theme Theme name ("defaultV2" or "modern")
#' @param primary Primary color hex code (optional)
#' @param mode Color mode ("light" or "dark")
#' @return A tagList containing survey dependencies and container
#' @importFrom htmltools tags tagList
#' @importFrom sass sass
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs hidden
#' @export
surveyUI <- function(id = "surveyContainer", theme = "defaultV2", primary = "#003594", mode = "light") {
  css_file <- switch(theme,
                     "defaultV2" = "https://unpkg.com/survey-core@1.9.116/defaultV2.min.css",
                     "modern" = "https://unpkg.com/survey-core@1.9.116/modern.min.css"
  )

  # Define dark mode colors
  dark_bg <- if(mode == "dark") "#1a1a1a" else "#ffffff"
  dark_text <- if(mode == "dark") "#e0e0e0" else "#404040"
  dark_container_bg <- if(mode == "dark") "#2d2d2d" else "#ffffff"
  dark_border <- if(mode == "dark") "#404040" else "#e0e0e0"

  # Custom CSS for dark mode
  dark_mode_css <- sprintf("
    body {
      background-color: %s !important;
      color: %s !important;
    }
    #surveyResponseContainer {
      background-color: %s;
      color: %s;
      border: 1px solid %s;
      padding: 20px;
      border-radius: 4px;
    }
    #surveyResponseContainer h4 {
      color: %s;
      margin-bottom: 15px;
    }
    .dataTables_wrapper {
      background-color: %s;
      color: %s;
      padding: 10px;
    }
    .dataTables_info,
    .dataTables_length,
    .dataTables_filter {
      color: %s !important;
    }
    .dataTable {
      border-color: %s !important;
    }
    .dataTable th,
    .dataTable td {
      background-color: %s !important;
      color: %s !important;
      border-color: %s !important;
    }
  ", dark_bg, dark_text, dark_container_bg, dark_text, dark_border,
                           dark_text, dark_container_bg, dark_text, dark_text,
                           dark_border, dark_container_bg, dark_text, dark_border)

  tagList(
    tags$head(
      tags$script(src = paste0("https://unpkg.com/survey-jquery/survey.jquery.min.js")),
      tags$link(rel = "stylesheet", href = css_file),
      tags$style(sass::sass(survey_css(primary = primary, mode = mode))),
      tags$style(dark_mode_css),
      tags$script(HTML(survey_single_js())),
      shinyjs::useShinyjs()
    ),
    div(id = id),
    conditionalPanel(
      condition = "input.surveyComplete == true && output.showResponseTable == true",
      div(
        id = "surveyResponseContainer",
        style = "margin-top: 30px;",
        h4("Your Response:"),
        DT::dataTableOutput("surveyResponseTable")
      )
    )
  )
}
