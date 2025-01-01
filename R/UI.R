#' Create Survey UI with Message Components
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
#' @export
surveyUI <- function(id = "surveyContainer", theme = "defaultV2",
                     primary = "#003594", mode = "light") {
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
  gradient_color2 <- if (mode == "dark") adjust_hex(primary, 15, lighten = TRUE) else "#00AD6E"

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

  # Message UI CSS
  message_ui_css <- sprintf(
    "
    .message-container {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%%;
      height: 100%%;
      background: %s;
      opacity: 0.50;
      z-index: 9998;
      display: flex;
      justify-content: center;
      align-items: center;
      flex-direction: column;
      font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', sans-serif;
      font-size: clamp(2rem, 2vw, 1.25rem);
      color: %s;
      text-align: center;
      letter-spacing: -0.01em;
      transition: opacity 0.3s ease;
    }

    #surveyContainer {
      position: relative;
      z-index: 9999;
    }

    .sv-completedpage {
      position: relative;
      z-index: 10000;
      background-color: %s !important;
      opacity: 1 !important;
    }

    #surveyResponseContainer {
      position: relative;
      z-index: 10000;
      background-color: %s;
      opacity: 1 !important;
    }

    .loading-spinner {
      width: 80px;
      height: 80px;
      position: relative;
      margin-bottom: 20px;
    }

    .loading-spinner::before,
    .loading-spinner::after {
      content: '';
      position: absolute;
      inset: 0;
      border: 4px solid %s;
      border-radius: 50%%;
      animation: pulse 2s cubic-bezier(0.4, 0, 0.2, 1) infinite;
    }

    .loading-spinner::after {
      animation-delay: -1s;
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

    .sv-root-modern {
      background-color: %s;
      position: relative;
      z-index: 9999;
    }
    ",
    dark_bg,        # Background color
    dark_text,      # Text color
    dark_container_bg, # Completed page background
    dark_container_bg, # Response container background
    gradient_color2,   # Spinner color
    dark_container_bg  # Survey root background
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
      htmltools::tags$script(HTML(survey_single_js())),
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
