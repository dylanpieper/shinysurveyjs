#' Create Survey UI with CSS and Message Components
#'
#' @param id Optional id for the survey div container (default is "surveyContainer")
#' @param theme Theme name ("defaultV2" or "modern")
#' @param primary Primary color hex code (optional)
#' @param mode Color mode ("light" or "dark")
#' @return A tagList containing survey dependencies and container
#' @importFrom shiny div conditionalPanel h4 tags
#' @importFrom htmltools tagList HTML
#' @importFrom sass sass
#' @importFrom DT dataTableOutput
#' @importFrom shinyjs useShinyjs
survey_ui <- function(id, theme, primary, mode, cookie_expiration_days) {
  css_file <- switch(theme,
    "defaultV2" = "https://unpkg.com/survey-core@1.9.116/defaultV2.min.css",
    "modern" = "https://unpkg.com/survey-core@1.9.116/modern.min.css"
  )

  # Define dark mode colors
  dark_bg <- if (mode == "dark") "#1a1a1a" else "#ffffff"
  dark_text <- if (mode == "dark") "#e0e0e0" else "#404040"
  dark_container_bg <- if (mode == "dark") "#2d2d2d" else "#ffffff"
  dark_border <- if (mode == "dark") "#404040" else "#e0e0e0"

  # Get gradient colors based on mode
  gradient_color1 <- if (mode == "dark") primary else "#DBEEFF"
  gradient_color2 <- if (mode == "dark") adjust_hex(primary, 15) else "#00AD6E"

  # Define dark mode CSS
  dark_mode_css <- sprintf(
    "
    body {
      background-color: %s !important;
      color: %s !important;
    }

    .sv-root-modern {
      background-color: %s;
      color: %s;
    }

    .sv-body {
      background-color: %s;
      color: %s;
    }

    .sv-container-modern {
      color: %s;
    }

    #surveyResponseContainer {
      background-color: %s;
      color: %s;
      border: 1px solid %s;
      padding: 20px;
      border-radius: 4px;
    }

    .dataTables_wrapper {
      background-color: %s;
      color: %s;
      padding: 10px;
    }

    .sv-checkbox-material {
      border-color: %s;
    }

    .error-message {
      font-size: clamp(2rem, 2.5vw, 1.5rem);
      font-weight: 600;
      color: %s;
      padding: 1rem 2rem;
      border-radius: 8px;
      background: %s;
      box-shadow: 0 2px 8px rgba(0, 53, 148, 0.15);
      max-width: 90%%;
      margin: 0 auto;
    }

    .sv-footer.sv-action-bar {
      background-color: %s;
    }

    .sv-question {
      background-color: %s;
      border-color: %s;
    }",
    dark_bg, dark_text, # body
    dark_container_bg, dark_text, # sv-root-modern
    dark_container_bg, dark_text, # sv-body
    dark_text, # sv-container-modern
    dark_container_bg, dark_text, dark_border, # surveyResponseContainer
    dark_container_bg, dark_text, # dataTables_wrapper
    dark_border, # checkbox
    dark_text, dark_container_bg, # error-message
    dark_container_bg, # footer
    dark_container_bg, dark_border # question
  )

  message_ui_css <- sprintf(
    "
    .message-container {
      position: fixed;
      top: 50%%;
      left: 50%%;
      transform: translate(-50%%, -50%%);
      width: auto;
      max-width: min(600px, 90%%);
      min-width: 280px;
      height: auto;
      max-height: 90vh;
      background: transparent;
      z-index: 10000;
      display: flex;
      justify-content: center;
      align-items: center;
      flex-direction: column;
      font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif;
      font-size: clamp(2rem, 3vw + 0.5rem, 1.5rem);
      color: %s;
      text-align: center;
      letter-spacing: -0.01em;
      transition: opacity 0.3s ease;
      padding: 1.5rem;
      box-sizing: border-box;
      overflow-y: auto;
    }

    .sv-root-modern {
      background-color: %s;
      position: relative;
      z-index: 1;
    }

    .sv-body {
      background-color: %s;
      color: %s;
    }

    .sv-container-modern {
      color: %s;
    }

    #surveyResponseContainer {
      position: relative;
      z-index: 1;
      background-color: %s;
      opacity: 1 !important;
    }

    .loading-spinner {
      width: min(80px, 15vw);
      height: min(80px, 15vw);
      position: relative;
      margin: clamp(0.5rem, 3vw, 1.5rem) auto;
    }

    .loading-spinner::before,
    .loading-spinner::after {
      content: '';
      position: absolute;
      inset: 0;
      border: min(4px, 0.8vw) solid transparent;
      border-radius: 50%%;
      animation: pulse 2s cubic-bezier(0.4, 0, 0.2, 1) infinite;
    }

    .loading-spinner::before {
      border-color: %s; /* Primary color */
    }

    .loading-spinner::after {
      border-color: %s; /* Lighter variant of primary */
      animation-delay: -1s;
    }

    /* Error message containment */
    .error-message {
      font-size: clamp(2rem, 4vw, 1.5rem);
      font-weight: 600;
      color: %s;
      padding: clamp(0.5rem, 2vw, 1rem);
      border-radius: 8px;
      background: %s;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
      width: 100%%;
      word-wrap: break-word;
      overflow-wrap: break-word;
    }

    @keyframes pulse {
      0%% {
        transform: scale(0.8);
        opacity: 1;
      }
      50%% {
        transform: scale(1.2);
        opacity: 0.2;
      }
      100%% {
        transform: scale(0.8);
        opacity: 1;
      }
    }

    /* Media query for better mobile containment */
    @media screen and (max-width: 480px) {
      .message-container {
        padding: 0.75rem;
      }

      .message-container > div {
        padding: 0.5rem;
      }

      .loading-spinner {
        margin: 0.5rem auto;
      }
    }
    ",
    dark_text, # Text color for message container
    dark_container_bg, # Background color for sv-root-modern
    dark_container_bg, # Background color for sv-body
    dark_text, # Text color for sv-body
    dark_text, # Text color for sv-container-modern
    dark_container_bg, # Background color for surveyResponseContainer
    primary, # Primary color for first spinner
    adjust_hex(primary, 20), # Lighter variant of primary for second spinner
    dark_text, # Text color for error message
    dark_container_bg # Background color for error message
  )

  # Animation keyframes
  animation_css <- "
    @keyframes spin {
      0% { transform: rotate(0deg) scale(1); }
      50% { transform: rotate(180deg) scale(0.95); }
      100% { transform: rotate(360deg) scale(1); }
    }
    @keyframes ellipsis {
      0% { content: '.'; }
      33% { content: '..'; }
      66% { content: '...'; }
      100% { content: '.'; }
    }
    .loading-text {
      position: relative;
      display: inline-block;
    }
    .loading-text::after {
      content: '...';
      position: absolute;
      animation: ellipsis 1.5s infinite;
      margin-left: 4px;
    }
  "

  # Combine all UI elements
  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$script(src = "https://unpkg.com/survey-jquery/survey.jquery.min.js"),
      htmltools::tags$link(rel = "stylesheet", href = css_file),
      htmltools::tags$style(sass::sass(survey_css(primary = primary, mode = mode))),
      htmltools::tags$style(HTML(dark_mode_css)),
      htmltools::tags$style(HTML(message_ui_css)),
      htmltools::tags$style(HTML(animation_css)),
      htmltools::tags$script(HTML(survey_single_js(cookie_expiration_days = cookie_expiration_days))),
      shinyjs::useShinyjs()
    ),
    # Message containers
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
    ),
    div(
      id = "savingDataMessage",
      class = "message-container",
      style = "display: none;",
      div(class = "loading-spinner"),
      div(class = "loading-text", "Saving Response")
    ),
    # Main survey container
    shiny::div(id = id)
  )
}

#' Create Survey UI Wrapper
#'
#' Creates a Shiny UI wrapper for displaying a survey with an optional response table.
#' The UI includes a loading spinner and conditional panels based on survey state.
#'
#' @param theme The theme configuration for styling the survey
#' @param theme_color Primary color used for UI elements like the loading spinner
#' @param theme_mode The theme mode (e.g., 'light' or 'dark')
#' @return A Shiny UI definition
#'
#' @importFrom shiny fluidPage conditionalPanel div h3
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT DTOutput
#'
#' @export
survey_ui_wrapper <- function(id, theme, theme_color, theme_mode, cookie_expiration_days) {
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    survey_ui(id = id, theme = theme, primary = theme_color, mode = theme_mode, cookie_expiration_days = cookie_expiration_days),
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
