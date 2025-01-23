#' Create Survey UI with CSS and Message Components
#'
#' Builds the UI components for a SurveyJS-based survey, including CSS styling,
#' message panels, and data display elements.
#'
#' @param id String. HTML ID for the survey container div. Default: "surveyContainer".
#' @param theme String. SurveyJS theme, either "defaultV2" or "modern".
#'   Default: "defaultV2".
#' @param primary String. Hex color code for primary theme customization.
#' @param mode String. Color mode, either "light" or "dark". Default: "light".
#' @param cookie_expiration_days Numeric. Number of days to retain survey cookies.
#'   Default: 7.
#' @param custom_css String. Custom CSS rules to append to the theme.
#'
#' @return A tagList containing survey dependencies, container div, message panels,
#'   and optional data display components.
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
#' @param id String. ID for the survey div container.
#' @param theme String. SurveyJS theme, either "defaultV2" or "modern".
#'   Default: "defaultV2".
#' @param theme_color String. Hex color code for primary theme customization.
#' @param theme_mode String. Color mode, either "light" or "dark".
#'   Default: "light".
#' @param cookie_expiration_days Numeric. Number of days to retain survey cookies.
#'   Default: 7.
#' @param custom_css String. Custom CSS rules to append to the theme.
#'
#' @return A Shiny UI definition containing survey container, loading spinner,
#'   and optional response table.
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
