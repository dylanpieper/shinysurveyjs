#' Generate Complete Survey Theme
#'
#' Creates a comprehensive theme including CSS variables and styling for all
#' SurveyJS components based on specified parameters.
#'
#' @param theme String. Base theme template, either "defaultV2" or "modern".
#'   Default: "defaultV2".
#' @param primary String. Hex color code for primary theme color (e.g., "#1ab394").
#' @param primary_foreground String. Hex color code for text on primary elements.
#'   If not specified, automatically determined for contrast.
#' @param mode String. Color scheme mode, either "light" or "dark".
#'   Default: "light".
#' @param custom_css String. Additional CSS rules to append to the theme.
#'   Default: `NULL`.
#'
#' @return String containing complete CSS stylesheet for survey styling
#'
#' @keywords internal
generate_survey_theme <- function(
    theme = "defaultV2",
    primary = "#003594",
    mode = "light",
    custom_css = NULL) {
  # Validate inputs
  stopifnot(
    is.character(primary),
    mode %in% c("light", "dark"),
    theme %in% c("defaultV2", "modern")
  )

  # Calculate variant colors
  primary_light <- adjust_hex(primary, 25)
  primary_dark <- adjust_hex(primary, -25)
  button_text_color <- get_contrast_color(primary)
  button_text_color_light <- get_contrast_color(primary_light)

  # Set mode-specific colors
  if (mode == "dark") {
    background <- "#404040"
    foreground <- "#e0e0e0"
    input_background <- "#404040"
    border_color <- "#888888"
    hover_background <- "#2a2a2a"
    header_background <- "#2d2d2d"
    disabled_color <- "#808080"
    text_border <- "#404040"
    container_background <- "#2d2d2d"
  } else {
    background <- "#ffffff"
    foreground <- "#404040"
    input_background <- "#ffffff"
    border_color <- "#e0e0e0"
    hover_background <- "#f5f5f5"
    header_background <- "#e7e7e7"
    disabled_color <- "#dbdbdb"
    text_border <- "#d4d4d4"
    container_background <- "#f9f9f9"
  }

    # Base theme CSS
    base_css <- if (theme == "modern") {
      # [Modern theme CSS remains unchanged]
      sprintf(
        "
.sv-root-modern {
    /* Primary colors */
    --main-color: %s;
    --main-hover-color: %s;
    /* [Rest of modern theme variables remain the same] */
}",
        primary, primary_light, foreground, button_text_color,
        background, hover_background, header_background,
        sprintf("rgba(%s, 0.2)", paste(hex_to_rgb(primary), collapse = ", ")),
        text_border, border_color, primary, primary,
        adjust_hex(primary, 40), disabled_color,
        button_text_color, foreground
      )
    } else {
      sprintf(
        "
:root {
    --primary: %s;
    --primary-light: %s;
    --primary-dark: %s;
    --primary-foreground: %s;
    --background: %s;
    --foreground: %s;
    --input-background: %s;
    --border-color: %s;
    --hover-background: %s;
    --container-background: %s;

    /* Component mappings for default theme */
    --sd-button-primary-background: var(--primary);
    --sd-button-primary-text-color: %s;
    --sd-navigation-button-background: var(--primary);
    --sd-navigation-button-text-color: %s;
    --sd-navigation-button-hover: var(--primary-light);
}

/* Default theme specific styles */
.sv-components-row {
    border-top: 2px solid var(--primary) !important;
}",
  primary, primary_light, primary_dark, button_text_color,
  background, foreground, input_background, border_color, hover_background, container_background,
  button_text_color, button_text_color
      )
    }

# Base styles
base_styles <- sprintf(
  "
/* Base styles */
body {
    background-color: %s !important;
    color: %s !important;
    font-size: 16px !important;
}

/* Headers */
.sv-root-modern h1,
.sv-root-modern h2,
.sv-root-modern h3,
.sv-root-modern h4,
.sv-root-modern h5,
.sv-root-modern h6,
.sv-root-modern .sv-title,
.sv-root-modern .sv-header__text,
.sv-root-modern .sd-header__text .sd-title,
.sv-root-modern .sv-header__text h3,
.sv-root-modern .sv-container-modern .sv-title,
.sv-container-modern .sv-title,
#surveyResponseContainer h3 {
    color: var(--foreground) !important;
}

/* Component column background */
.sv-components-column {
    background-color: %s;
}

/* Dark mode specific styles */
body[data-theme=\"dark\"] .sv-components-column {
    background-color: %s !important;
}

.sv-body {
    background-color: %s;
    color: %s;
}",
background, foreground,
container_background,
container_background,
container_background, foreground
)

# Container styles
container_styles <- if (theme == "modern") {
  # [Modern theme container styles remain unchanged]
  sprintf(
    "
/* Completed page and container styles */
.sd-body.sd-completedpage,
.sv-body.sv-completedpage {
    width: 100%% !important;
    margin: 0 auto !important;
    padding: 2rem !important;
    box-sizing: border-box !important;
    height: auto !important;
    display: flex !important;
    place-items: center !important;
    flex-wrap: wrap;
    flex-direction: row-reverse;
    justify-content: space-evenly;
    align-content: center;
    border-top: 2px solid var(--primary) !important;
}

.sv-container-modern,
.sv-body {
    max-width: 1200px !important;
    margin: 0 auto !important;
    padding: 1rem !important;
    box-sizing: border-box !important;
    color: %s;
}",
foreground
  )
} else {
  sprintf(
    "
/* Completed page and container styles */
.sd-body.sd-completedpage,
.sv-body.sv-completedpage {
    width: 100%% !important;
    margin: 0 auto !important;
    padding: 2rem !important;
    box-sizing: border-box !important;
    height: auto !important;
    display: flex !important;
    place-items: center !important;
    min-height: 32vh !important;
    flex-direction: column-reverse;
    background-color: var(--container-background) !important;
    border-top: 2px solid var(--primary) !important;
}

.sv-root-modern,
.sd-root-modern {
    width: 100%% !important;
    overflow-x: hidden !important;
}

.sv-container-modern,
.sv-body {
    max-width: 1200px !important;
    margin: 0 auto !important;
    padding: 1rem !important;
    box-sizing: border-box !important;
    color: %s;
}

.sv-question {
    background-color: %s;
    border: 1px solid %s;
    border-radius: 4px;
    padding: 16px;
}

.sv-question:hover {
    background-color: %s;
}

.sv-question.sv-question--selected {
    border-color: %s;
    background-color: %s;
}

.sv-checkbox-material {
    border-color: %s;
}

.sv-footer.sv-action-bar {
    background-color: %s;
    max-width: 1200px !important;
    margin: 0 auto !important;
}",
foreground,
container_background, border_color,
hover_background,
primary, hover_background,
border_color,
container_background
  )
}

# Button styles
button_styles <- if (theme == "defaultV2") {
  sprintf(
    "
/* Default theme button styles */
.sd-btn {
    background-color: var(--primary) !important;
    color: %s !important;
    border: none !important;
    transition: background-color 0.2s ease-in-out !important;
}

.sd-btn:hover {
    background-color: var(--primary-light) !important;
    color: %s !important;
    opacity: 0.9;
}

.sd-btn:disabled {
    background-color: var(--border-color) !important;
    opacity: 0.6;
}

/* Navigation buttons */
.sd-navigation__next-btn,
.sd-navigation__prev-btn {
    background-color: var(--primary) !important;
    color: %s !important;
    border: none !important;
    transition: background-color 0.2s ease-in-out !important;
}

.sd-navigation__next-btn:hover,
.sd-navigation__prev-btn:hover {
    background-color: var(--primary-light) !important;
    color: %s !important;
}

/* Complete button */
.sd-btn--action[value=\"Complete\"] {
    background-color: var(--primary) !important;
    color: %s !important;
    border: none !important;
    transition: background-color 0.2s ease-in-out !important;
}

.sd-btn--action[value=\"Complete\"]:hover {
    background-color: var(--primary-light) !important;
    color: %s !important;
}

/* Rating items */
.sd-rating-item--selected {
    background-color: var(--primary) !important;
    color: %s !important;
}",
button_text_color,
button_text_color, # Use same color for hover
button_text_color,
button_text_color, # Use same color for hover
button_text_color,
button_text_color, # Use same color for hover
button_text_color
  )
} else {
  sprintf(
    "
/* Modern theme button styles */
.sv-btn.sv-action-bar-item--secondary {
    color: %s !important;
}

.sv-btn.sv-action-bar-item--secondary:hover {
    background-color: %s !important;
    color: %s !important;
    opacity: 0.9;
}

.sv-footer__complete-btn,
.sv-btn--navigation {
    color: %s !important;
}

.sv-footer__complete-btn:hover,
.sv-btn--navigation:hover {
    background-color: %s !important;
    color: %s !important;
    opacity: 0.9;
}

/* Modern theme button sizes */
.sv-root-modern .sv-btn,
.sv-root-modern .sv-footer__complete-btn,
.sv-root-modern .sv-nav-btn {
    padding: 10px 25px !important;
    font-size: 2rem !important;
    min-height: 48px !important;
    line-height: 1.4 !important;
}

.sv-root-modern .sv-footer__complete-btn {
    font-weight: 600 !important;
}

.sv-root-modern .sv-btn--navigation {
    min-width: 120px !important;
}",
button_text_color,
primary_light,
button_text_color, # Use same color for hover
button_text_color,
primary_light,
button_text_color # Use same color for hover
  )
}

# Message container CSS
message_css <- sprintf(
  '
/* Message containers */
.message-container {
    position: fixed;
    top: 50%%;
    left: 50%%;
    transform: translate(-50%%, -50%%);
    width: 90%%;
    max-width: 600px;
    min-width: 280px;
    height: auto;
    max-height: 90vh;
    background: transparent;
    z-index: 10000;
    display: flex;
    justify-content: center;
    align-items: center;
    flex-direction: column;
    font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", sans-serif;
    font-size: 20px;
    color: %s;
    text-align: center;
    letter-spacing: -0.01em;
    transition: opacity 0.3s ease;
    padding: 24px;
    box-sizing: border-box;
    overflow-y: auto;
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

.loading-spinner {
    width: 80px;
    height: 80px;
    position: relative;
    margin: 16px auto;
}

.loading-spinner::before,
.loading-spinner::after {
    content: "";
    position: absolute;
    inset: 0;
    border: 4px solid transparent;
    border-radius: 50%%;
    animation: pulse 2s cubic-bezier(0.4, 0, 0.2, 1) infinite;
}

.loading-spinner::before {
    border-color: %s;
}

.loading-spinner::after {
    border-color: %s;
    animation-delay: -1s;
}

.loading-text {
    position: relative;
    display: inline-block;
    font-size: 24px;
}

.error-message {
    font-size: 24px;
    font-weight: 600;
    color: %s;
    padding: 16px;
    border-radius: 8px;
    background: %s;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
    width: 100%%;
    word-wrap: break-word;
    overflow-wrap: break-word;
}

@media screen and (max-width: 480px) {
    .message-container {
        padding: 12px;
        width: 95%%;
        font-size: 16px;
    }

    .loading-spinner {
        width: 60px;
        height: 60px;
    }

    .loading-spinner::before,
    .loading-spinner::after {
        border-width: 3px;
    }

    .loading-text {
        font-size: 18px;
    }

    .error-message {
        font-size: 18px;
        padding: 12px;
    }
}',
foreground,
primary,
adjust_hex(primary, 20),
foreground, container_background
)

# Combine all CSS sections
paste(
  base_css,
  base_styles,
  button_styles,
  container_styles,
  message_css,
  if (!is.null(custom_css)) {
    custom_css
  },
  sep = "\n\n"
)
}
