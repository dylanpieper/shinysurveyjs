#' Create Survey UI with CSS and Message Components
#'
#' @param id ID for the survey div container (e.g., "surveyContainer")
#' @param theme Theme name ("defaultV2" or "modern")
#' @param primary Primary color hex code (optional)
#' @param mode Color mode ("light" or "dark")
#' @param cookie_expiration_days Number of days to keep cookies for survey data
#' @param custom_css Optional custom CSS to append to the theme
#' @return A tagList containing survey dependencies and container
#'
#' @importFrom shiny div conditionalPanel h4 tags
#' @importFrom htmltools tagList HTML
#' @importFrom sass sass
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs
#'
#' @keywords internal
survey_ui <- function(id, theme, primary, mode, cookie_expiration_days, custom_css) {
  css_file <- switch(theme,
                     "defaultV2" = "https://unpkg.com/survey-core@1.9.116/defaultV2.min.css",
                     "modern" = "https://unpkg.com/survey-core@1.9.116/modern.min.css"
  )

  # Combine all UI elements
  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$script(src = "https://unpkg.com/survey-jquery/survey.jquery.min.js"),
      htmltools::tags$link(rel = "stylesheet", href = css_file),
      htmltools::tags$style(sass::sass(
        generate_survey_theme(
          theme = theme,
          primary = primary,
          mode = mode,
          custom_css = custom_css
        )
      )),
      htmltools::tags$script(HTML(survey_single_js(cookie_expiration_days = cookie_expiration_days))),
      shinyjs::useShinyjs()
    ),
    # Main survey container
    shiny::div(id = id),

    # Saving message after survey container
    div(
      id = "savingDataMessage",
      class = "message-container saving-message",
      style = "display: none;",
      div(class = "loading-spinner"),
      div(class = "loading-text", "Saving Response")
    ),

    # Other message containers
    div(
      id = "waitingMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "loading-spinner"),
      div(class = "loading-text", "Loading Survey")
    ),
    div(
      id = "surveyNotFoundMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "error-message", "⚠️ Survey Not Found")
    ),
    div(
      id = "surveyNotDefinedMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "error-message", "❌ Survey Undefined")
    ),
    div(
      id = "invalidQueryMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "error-message", "❌ Invalid Query")
    ),
    div(
      id = "invalidDataMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "error-message", "❌ Invalid Data")
    ),
    div(
      id = "inactiveSurveyMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "error-message", "⏰ Inactive Survey")
    ),
    div(
      id = "surveyNotStartedMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "error-message", "⏰ Survey Not Started")
    ),
    div(
      id = "surveyEndedMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "error-message", "⏰ Survey Ended")
    )
  )
}

#' Create Survey UI Wrapper
#'
#' Creates a Shiny UI wrapper for displaying a survey with an optional response table.
#' The UI includes a loading spinner and conditional panels based on survey state.
#'
#' @param id The ID for the survey div container
#' @param theme The theme configuration for styling the survey
#' @param theme_color Primary color used for UI elements like the loading spinner
#' @param theme_mode The theme mode (e.g., 'light' or 'dark')
#' @param cookie_expiration_days Number of days to keep cookies for survey data
#' @param custom_css Optional custom CSS to append to the theme
#' @return A Shiny UI definition
#'
#' @importFrom shiny fluidPage conditionalPanel div h3
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput
#'
#' @keywords internal
survey_ui_wrapper <- function(id, theme, theme_color, theme_mode, cookie_expiration_days, custom_css) {
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    survey_ui(id = id, theme = theme, primary = theme_color, mode = theme_mode, cookie_expiration_days = cookie_expiration_days, custom_css = custom_css),
    shiny::conditionalPanel(
      condition = "output.showResponseTable",
      shiny::div(
        id = "surveyResponseContainer",
        style = "display: none;",
        #shiny::h3("Your Response:"),
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
