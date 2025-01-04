#' Single Survey JavaScript
#'
#' Returns the JavaScript code needed to initialize and handle a single survey in a Shiny application.
#' This JavaScript code sets up event handlers for survey completion and enables communication between the survey and Shiny.
#'
#' @return A character string containing the JavaScript code for survey initialization
#' @keywords internal
survey_single_js <- function() {
  "$(document).ready(function() {
    var survey;
    const COOKIE_NAME = 'surveyProgress';
    const COOKIE_EXPIRATION_DAYS = 7;

    // Utility functions for cookie handling
    function setCookie(name, value, days) {
        const expires = new Date();
        expires.setTime(expires.getTime() + (days * 24 * 60 * 60 * 1000));
        document.cookie = name + '=' + encodeURIComponent(JSON.stringify(value)) + ';expires=' + expires.toUTCString() + ';path=/';
    }

    function getCookie(name) {
        const nameEQ = name + '=';
        const ca = document.cookie.split(';');
        for (let i = 0; i < ca.length; i++) {
            let c = ca[i];
            while (c.charAt(0) === ' ') c = c.substring(1, c.length);
            if (c.indexOf(nameEQ) === 0) {
                try {
                    return JSON.parse(decodeURIComponent(c.substring(nameEQ.length, c.length)));
                } catch (e) {
                    console.error('Error parsing cookie:', e);
                    return null;
                }
            }
        }
        return null;
    }

    function deleteCookie(name) {
        document.cookie = name + '=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/';
    }

    function saveSurveyProgress(survey) {
        const data = {
            data: survey.data,
            currentPageNo: survey.currentPageNo,
            timestamp: new Date().getTime()
        };
        setCookie(COOKIE_NAME, data, COOKIE_EXPIRATION_DAYS);
    }

    function loadSurveyProgress(survey) {
        const savedData = getCookie(COOKIE_NAME);
        if (savedData && savedData.data) {
            survey.data = savedData.data;
            if (savedData.currentPageNo !== undefined) {
                survey.currentPageNo = savedData.currentPageNo;
            }
            return true;
        }
        return false;
    }

    $('#surveyContainer').show();

    function initializeSurvey(surveyJSON) {
        try {
            if (typeof surveyJSON === 'string') {
                surveyJSON = JSON.parse(surveyJSON);
            }
            // Update document title if available
            if (surveyJSON.title) {
                document.title = surveyJSON.title;
            }
            // Clear the container without removing it
            $('#surveyContainer').empty();
            // Create the survey
            survey = new Survey.Model(surveyJSON);

            // Add auto-save functionality
            survey.onValueChanged.add(function(sender, options) {
                saveSurveyProgress(survey);
            });

            survey.onCurrentPageChanged.add(function(sender, options) {
                saveSurveyProgress(survey);
            });

            // Load saved progress if available
            const hasProgress = loadSurveyProgress(survey);

            survey.onComplete.add(function(result) {
                // Delete the progress cookie on completion
                deleteCookie(COOKIE_NAME);

                // First trigger completion to show loading state
                Shiny.setInputValue('surveyComplete', true);
                // Keep the container visible
                $('#surveyContainer').show();
                // Show saving message with spinner
                $('#savingDataMessage').show();
                // Set z-index for completion page
                $('.sv-completedpage').css({
                    'position': 'relative',
                    'z-index': '10000',
                    'opacity': '1'
                });
                Shiny.setInputValue('surveyData', JSON.stringify(result.data));
            });

            // Initialize jQuery Survey
            $('#surveyContainer').Survey({
                model: survey,
                onComplete: function(survey, options) {
                    // Ensure container stays visible after completion
                    $('#surveyContainer').show();
                }
            });

            // Add window unload handler to save progress
            $(window).on('beforeunload', function() {
                if (!survey.isCompleted) {
                    saveSurveyProgress(survey);
                }
            });

        } catch (error) {
            console.error('Error initializing survey:', error);
            console.error(error.stack);
            // Show error message if survey initialization fails
            $('#surveyNotDefinedMessage').show();
        }
    }

    Shiny.addCustomMessageHandler('loadSurvey', function(surveyJSON) {
        initializeSurvey(surveyJSON);
    });
});"
}

#' Survey CSS with Theme Support
#'
#' Returns the CSS code needed for a survey in a Shiny application with light/dark theme support.
#'
#' @param primary Primary color in hex format (default: "#003594")
#' @param primary_foreground Text color for primary elements (default: "#ffffff")
#' @param background Background color (default: "#ffffff")
#' @param foreground Main text color (default: "#404040")
#' @param mode Color mode selection: "light" or "dark" (default: "light")
#' @param css_string Optional custom CSS string to override default styles
#' @return A character string containing the CSS code
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Light mode
#' tags$script(survey_css())
#'
#' # Dark mode
#' tags$script(survey_css(mode = "dark"))
#'
#' # Custom dark mode colors
#' tags$script(survey_css(
#'   theme = "dark",
#'   primary = "#7289da",
#'   background = "#2c2f33"
#' ))
#' }
survey_css <- function(
    primary = "#003594",
    primary_foreground = "#ffffff",
    background = "#ffffff",
    foreground = "#404040",
    mode = "light",
    css_string = NULL) {
  if (!is.null(css_string)) {
    return(css_string)
  }

  # Function to lighten/darken hex color
  adjust_hex <- function(hex, percent = 25, lighten = TRUE) {
    hex <- gsub("^#", "", hex)
    r <- strtoi(substr(hex, 1, 2), 16)
    g <- strtoi(substr(hex, 3, 4), 16)
    b <- strtoi(substr(hex, 5, 6), 16)

    if (lighten) {
      r <- min(255, r + (255 - r) * percent / 100)
      g <- min(255, g + (255 - g) * percent / 100)
      b <- min(255, b + (255 - b) * percent / 100)
    } else {
      r <- max(0, r * (1 - percent / 100))
      g <- max(0, g * (1 - percent / 100))
      b <- max(0, b * (1 - percent / 100))
    }

    sprintf("#%02x%02x%02x", round(r), round(g), round(b))
  }

  # Set mode-specific colors
  if (mode == "dark") {
    if (background == "#ffffff") background <- "#1a1a1a"
    if (foreground == "#404040") foreground <- "#e0e0e0"
    primary_light <- adjust_hex(primary, 25, lighten = TRUE)
    primary_dark <- adjust_hex(primary, 25, lighten = FALSE)
    input_background <- "#2d2d2d"
    border_color <- "#404040"
    hover_background <- "#2a2a2a"
  } else {
    primary_light <- adjust_hex(primary, 25, lighten = TRUE)
    primary_dark <- adjust_hex(primary, 25, lighten = FALSE)
    input_background <- "#ffffff"
    border_color <- "#e0e0e0"
    hover_background <- "#f5f5f5"
  }

  # No gradient colors needed for complete button - using primary and primary-dark

  sprintf(
    "
.sd-root-modern {
    /* Main colors */
    --primary: %s;
    --primary-light: %s;
    --primary-dark: %s;
    --primary-foreground: %s;
    --background: %s;
    --foreground: %s;
    --input-background: %s;
    --border-color: %s;
    --hover-background: %s;
    /* Apply theme colors */
    background-color: var(--background);
    color: var(--foreground);
}

/* Base styles */
.sd-root-modern {
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

/* Fine tuning */
.sd-question {
    background-color: var(--input-background);
    border: 1px solid var(--border-color);
}

.sd-question:hover {
    background-color: var(--hover-background);
}

.sd-input {
    background-color: var(--input-background);
    border: 1px solid var(--border-color);
    color: var(--foreground);
}

.sd-input:focus {
    border-color: var(--primary);
    background-color: var(--input-background);
}

.sd-btn--action:hover {
    background-color: var(--primary-light);
}

.sd-question.sd-question--selected {
    border-color: var(--primary);
    background-color: var(--hover-background);
}

.sd-rating-item--selected {
    background-color: var(--primary) !important;
    color: var(--primary-foreground) !important;
}

.sd-header__text .sd-title {
    color: var(--foreground);
}

/* Main button styles */
.sd-btn {
    background-color: var(--primary) !important;
    color: var(--primary-foreground) !important;
    border: none !important;
}

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

/* HTML container padding */
.sd-html {
    padding: 20px;
}

/* Complete button special styling */
.sd-btn--action[value='Complete'] {
    background: var(--primary) !important;
    color: var(--primary-foreground) !important;
    border: none !important;
}

/* Hover effect for complete button */
.sd-btn--action[value='Complete']:hover {
    background: var(--primary-light) !important;
}",
    # Base colors
    primary, primary_light, primary_dark, primary_foreground,
    background, foreground, input_background, border_color, hover_background
  )
}
