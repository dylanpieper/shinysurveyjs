#' Generate Survey CSS with Theme Support
#'
#' Creates CSS code for styling survey components with support for light and dark themes.
#' The function generates CSS variables and styles for various survey components including
#' buttons, inputs, questions, and navigation elements.
#'
#' @param primary Character string specifying the primary color in hex format. This color
#'   is used for buttons, selected states, and interactive elements. Default is "#003594".
#' @param primary_foreground Character string specifying the text color for primary elements
#'   in hex format. This should provide sufficient contrast against the primary color.
#'   Default is "#ffffff".
#' @param mode Character string specifying the color theme. Must be either "light" or "dark".
#'   Default is "light".
#' @param css_string Optional character string containing custom CSS. If provided, the function
#'   returns this string instead of generating new CSS. Default is NULL.
#'
#' @return A character string containing the complete CSS code for survey styling.
#'
#' @details
#' The function generates CSS that includes:
#' * Root CSS variables for colors and themes
#' * Component-specific styles for questions, inputs, and buttons
#' * Theme-specific color variables for light and dark modes
#' * Interactive states (hover, focus, disabled)
#' * Special styling for navigation and completion buttons
#'
#' The CSS is structured using CSS variables (custom properties) to maintain
#' consistency and enable dynamic theme switching.
#'
#' @examples
#' # Generate default light theme CSS
#' css <- survey_css()
#'
#' # Generate dark theme CSS
#' dark_css <- survey_css(mode = "dark")
#'
#' # Custom primary color with dark theme
#' custom_css <- survey_css(
#'   primary = "#FF0000",
#'   primary_foreground = "#FFFFFF",
#'   mode = "dark"
#' )
#'
#' @keywords internal
survey_css <- function(
    primary = "#003594",
    primary_foreground = "#ffffff",
    mode = "light",
    css_string = NULL) {
  if (!is.null(css_string)) {
    return(css_string)
  }

  # Validate inputs
  stopifnot(
    is.character(primary),
    is.character(primary_foreground),
    mode %in% c("light", "dark")
  )

  # Calculate variant colors
  primary_light <- adjust_hex(primary, 25)
  primary_dark <- adjust_hex(primary, -25)

  # Set mode-specific colors
  if (mode == "dark") {
    background <- "#1a1a1a"
    foreground <- "#e0e0e0"
    input_background <- "#2d2d2d"
    border_color <- "#404040"
    hover_background <- "#2a2a2a"
  } else {
    background <- "#ffffff"
    foreground <- "#404040"
    input_background <- "#ffffff"
    border_color <- "#e0e0e0"
    hover_background <- "#f5f5f5"
  }

  # Create the CSS string
  sprintf(
    '
/* Root variables */
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
}

/* Base Survey Styles */
.sd-root-modern {
    /* Theme application */
    background-color: var(--background);
    color: var(--foreground);

    /* Component mappings */
    --sd-button-primary-background: var(--primary);
    --sd-button-primary-text-color: var(--primary-foreground);
    --sd-navigation-button-background: var(--primary);
    --sd-navigation-button-text-color: var(--primary-foreground);
    --sd-navigation-button-hover: var(--primary-light);
    --sd-checkbox-checked-background: var(--primary);
    --sd-radio-checked-background: var(--primary);
    --sd-rating-background: var(--primary);
    --sd-progress-bar-background: var(--primary);
    --sd-progress-bar-text: var(--primary-foreground);
    --sd-matrix-background: var(--primary-light);
    --sd-matrix-selected-background: var(--primary);
}

/* Question Styles */
.sd-question {
    background-color: var(--input-background);
    border: 1px solid var(--border-color);
}

.sd-question:hover {
    background-color: var(--hover-background);
}

.sd-question.sd-question--selected {
    border-color: var(--primary);
    background-color: var(--hover-background);
}

/* Input Styles */
.sd-input {
    background-color: var(--input-background);
    border: 1px solid var(--border-color);
    color: var(--foreground);
}

.sd-input:focus {
    border-color: var(--primary);
    background-color: var(--input-background);
}

/* Button Styles */
.sd-btn {
    background-color: var(--primary) !important;
    color: var(--primary-foreground) !important;
    border: none !important;
}

.sd-btn:hover {
    opacity: 0.9;
}

.sd-btn:disabled {
    background-color: var(--border-color) !important;
    opacity: 0.6;
}

/* Navigation Buttons */
.sd-navigation__next-btn,
.sd-navigation__prev-btn {
    background-color: var(--primary-dark) !important;
    color: var(--primary-foreground) !important;
    border: none !important;
}

.sd-btn:hover,
.sd-navigation__next-btn:hover,
.sd-navigation__prev-btn:hover {
    background-color: var(--primary) !important;
    opacity: 0.9;
}

.sd-btn:disabled,
.sd-navigation__next-btn:disabled,
.sd-navigation__prev-btn:disabled {
    background-color: var(--border-color) !important;
    opacity: 0.6;
}

/* Complete Button */
.sd-btn--action[value="Complete"] {
    background: var(--primary) !important;
    color: var(--primary-foreground) !important;
    border: none !important;
}

.sd-btn--action[value="Complete"]:hover {
    background: var(--primary-light) !important;
}

/* Rating Items */
.sd-rating-item--selected {
    background-color: var(--primary) !important;
    color: var(--primary-foreground) !important;
}

/* Headers */
.sd-header__text .sd-title {
    color: var(--foreground);
}

/* Action Buttons */
.sd-btn--action:hover {
    background-color: var(--primary-light);
}

/* HTML Container */
.sd-html {
    padding: 20px;
}',
    # Insert the variables
    primary, primary_light, primary_dark, primary_foreground,
    background, foreground, input_background, border_color, hover_background
  )
}
