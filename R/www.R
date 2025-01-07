#' Create JavaScript code for handling survey functionality
#'
#' @description
#' Generates JavaScript code that manages survey functionality including cookie handling,
#' survey initialization, and data persistence. The code includes robust error handling
#' and proper type definitions.
#'
#' @param cookie_expiration_days Numeric value specifying how many days the survey
#'   progress cookie should persist. Must be a positive number. Default is 7 days.
#'
#' @return A character string containing the generated JavaScript code.
#'
#' @details
#' The generated JavaScript code includes several key components:
#' * Cookie management (setting, getting, and deleting)
#' * Survey progress persistence
#' * Hidden field management
#' * Survey initialization and event handling
#' * Error reporting to Shiny
#'
#' The code uses proper type definitions and includes comprehensive error handling
#' throughout all operations.
#'
#' @examples
#' \dontrun{
#' js_code <- survey_single_js(cookie_expiration_days = 14)
#' }
#'
#' @export
survey_single_js <- function(cookie_expiration_days = 7) {
  # Input validation
  if (!is.numeric(cookie_expiration_days) || cookie_expiration_days < 1) {
    stop("cookie_expiration_days must be a positive number")
  }

  # Break down the JavaScript into manageable chunks
  js_prefix <- paste0(
    '$(document).ready(function() {
    // Core variables with proper typing
    /** @type {Survey.Model} */
    let survey = null;
    /** @type {Object.<string, any>} */
    let storedParams = {};
    let isInitializing = false;
    let initializationTimer = null;

    // Constants
    const COOKIE_NAME = "surveyProgress";
    const COOKIE_EXPIRATION_DAYS = ', cookie_expiration_days, ';
    const DEBOUNCE_DELAY = 100;
    const CONDITION_DELAY = 200;'
  )

  js_type_defs <- '
    // Type definitions and utility functions
    /** @typedef {Object.<string, any>} SurveyData */
    /** @typedef {Object} ParamValue
     * @property {*} value - The value to set
     * @property {string} text - The display text
     */
  '

  js_cookie_functions <- '
    function setCookie(name, value, days) {
        try {
            if (!name || typeof name !== "string") {
                throw new Error("Invalid cookie name");
            }
            const expires = new Date();
            expires.setTime(expires.getTime() + (days * 24 * 60 * 60 * 1000));
            const cookieValue = encodeURIComponent(JSON.stringify(value));
            document.cookie = `${name}=${cookieValue};expires=${expires.toUTCString()};path=/;SameSite=Strict`;
        } catch (error) {
            console.error("Error setting cookie:", error);
            Shiny.setInputValue("surveyError", {
                type: "CookieError",
                message: "Failed to set cookie",
                details: error.message
            });
        }
    }

    function getCookie(name) {
        try {
            if (!name || typeof name !== "string") {
                throw new Error("Invalid cookie name");
            }
            const nameEQ = name + "=";
            const cookies = document.cookie.split(";");

            for (const cookie of cookies) {
                let c = cookie.trim();
                if (c.indexOf(nameEQ) === 0) {
                    const cookieValue = decodeURIComponent(c.substring(nameEQ.length));
                    return JSON.parse(cookieValue);
                }
            }
            return null;
        } catch (error) {
            console.error("Error reading cookie:", error);
            return null;
        }
    }

    function deleteCookie(name) {
        try {
            if (!name || typeof name !== "string") {
                throw new Error("Invalid cookie name");
            }
            document.cookie = `${name}=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/`;
            document.cookie = `${name}=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/;domain=${window.location.hostname}`;
            document.cookie = `${name}=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/;domain=.${window.location.hostname}`;
        } catch (error) {
            console.error("Error deleting cookie:", error);
        }
    }
  '

  js_survey_functions <- '
    function saveSurveyProgress(survey) {
        try {
            if (!survey || !survey.data) {
                throw new Error("Invalid survey instance or data");
            }
            // Get the actual values for storage
            const dataToStore = { ...survey.data };
            Object.entries(storedParams).forEach(([paramName, paramValue]) => {
                if (paramValue?.value !== undefined) {
                    dataToStore[paramName] = paramValue.value;
                }
            });
            setCookie(COOKIE_NAME, dataToStore, COOKIE_EXPIRATION_DAYS);
        } catch (error) {
            console.error("Error saving survey progress:", error);
            Shiny.setInputValue("surveyError", {
                type: "ProgressError",
                message: "Failed to save survey progress",
                details: error.message
            });
        }
    }

    function setHiddenFieldsFromShiny(survey, params) {
        if (!survey) {
            console.error("Invalid survey instance");
            return;
        }

        const paramsToUse = Object.keys(params || {}).length > 0 ? params : storedParams;
        console.log("Setting hidden fields with params:", paramsToUse);

        const setValueWithRetry = (paramName, paramData, retries = 3) => {
            const attempt = () => {
                try {
                    const question = survey.getQuestionByName(paramName);
                    if (question) {
                        // For HTML display, set the text as the value
                        if (paramData.text !== undefined) {
                            question.value = paramData.text;
                        } else {
                            question.value = paramData;
                        }

                        // Store the actual value separately for data handling
                        if (paramData.value !== undefined) {
                            question._internalValue = paramData.value;
                        }
                    }
                    return true;
                } catch (error) {
                    console.warn(`Attempt to set ${paramName} failed:`, error);
                    return false;
                }
            };

            const retryWithDelay = (retriesLeft) => {
                if (retriesLeft <= 0) {
                    console.error(`Failed to set value for ${paramName} after all retries`);
                    return;
                }

                setTimeout(() => {
                    if (!attempt()) {
                        retryWithDelay(retriesLeft - 1);
                    }
                }, DEBOUNCE_DELAY);
            };

            if (!attempt()) {
                retryWithDelay(retries - 1);
            }
        };

        try {
            Object.entries(paramsToUse).forEach(([paramName, paramValue]) => {
                if (!paramName) return;

                const valueToSet = paramValue?.value !== undefined ?
                    { text: paramValue.text, value: paramValue.value } :
                    paramValue;

                setValueWithRetry(paramName, valueToSet);
            });

            setTimeout(() => {
                if (typeof survey.runConditions === "function") {
                    survey.runConditions();
                }
            }, CONDITION_DELAY);
        } catch (error) {
            console.error("Error in setHiddenFieldsFromShiny:", error);
            Shiny.setInputValue("surveyError", {
                type: "HiddenFieldsError",
                message: "Failed to set hidden fields",
                details: error.message
            });
        }
    }
  '

  js_initialize_survey <- '
    function initializeSurvey(data) {
        try {
            if (!data) {
                throw new Error("No survey data provided");
            }

            console.log("Survey data received:", data);

            // Clear existing survey if it exists
            if (survey) {
                try {
                    survey.dispose();
                } catch (e) {
                    console.warn("Error disposing existing survey:", e);
                }
                survey = null;
            }

            // Clear survey container
            $("#surveyContainer").empty();

            let surveyJSON;
            if (typeof data === "object") {
                surveyJSON = data.survey || data;
                if (data.params) {
                    storedParams = data.params;
                    console.log("Stored params:", storedParams);
                }
            } else if (typeof data === "string") {
                try {
                    surveyJSON = JSON.parse(data);
                } catch (error) {
                    throw new Error("Invalid survey JSON format: " + error.message);
                }
            } else {
                throw new Error("Invalid survey data format");
            }

            survey = new Survey.Model(surveyJSON);

            const savedData = getCookie(COOKIE_NAME);
            if (savedData) {
                survey.data = savedData;
            }

            setHiddenFieldsFromShiny(survey, storedParams);

            let valueChangeTimeout;
            survey.onValueChanged.add((sender, options) => {
                console.log("Value changed:", options.name, options.value);

                clearTimeout(valueChangeTimeout);
                valueChangeTimeout = setTimeout(() => {
                    saveSurveyProgress(survey);
                    if (typeof survey.runConditions === "function") {
                        survey.runConditions();
                    }
                }, DEBOUNCE_DELAY);
            });

            // Add completion handler
            survey.onComplete.add((result) => {
                try {
                    // 1. Immediately show saving message and trigger completion state
                    document.getElementById("savingDataMessage").style.display = "block";
                    Shiny.setInputValue("surveyComplete", true);

                    // 2. Create a promise chain for the remaining operations
                    Promise.resolve()
                        .then(() => {
                            // Prepare the data
                            const responses = {};
                            for (const [key, value] of Object.entries(result.data)) {
                                if (!["data", "currentPageNo", "timestamp"].includes(key)) {
                                    const question = survey.getQuestionByName(key);
                                    responses[key] = question?._internalValue ?? value;
                                }
                            }
                            return responses;
                        })
                        .then((responses) => {
                            // Send the data
                            Shiny.setInputValue("surveyData", responses);
                        })
                        .then(() => {
                            // Handle cookie cleanup
                            deleteCookie(COOKIE_NAME);
                            return new Promise(resolve => {
                                setTimeout(() => {
                                    const remainingCookie = getCookie(COOKIE_NAME);
                                    if (remainingCookie) {
                                        deleteCookie(COOKIE_NAME);
                                    }
                                    resolve();
                                }, 100);
                            });
                        })
                        .catch(error => {
                            console.error("Error in completion handler:", error);
                            Shiny.setInputValue("surveyError", {
                                type: "CompletionError",
                                message: "Error processing survey completion",
                                details: error.message
                            });
                        });
                        console.log("Processed data");
                } catch (error) {
                    console.error("Error in completion handler:", error);
                    Shiny.setInputValue("surveyError", {
                        type: "CompletionError",
                        message: "Error processing survey completion",
                        details: error.message
                    });
                }
            });

            $("#surveyContainer").Survey({
                model: survey,
                onAfterRenderSurvey: () => {
                    console.log("Loaded survey");
                }
            });

        } catch (error) {
            console.error("Survey initialization error:", error);
            isInitializing = false;
            Shiny.setInputValue("surveyError", {
                type: "InitializationError",
                message: "Failed to initialize survey",
                details: error.message
            });
        }
    }
  '

  js_handlers <- '
    Shiny.addCustomMessageHandler("loadSurvey", function(data) {
        if (isInitializing) {
            console.log("Survey initialization already in progress, skipping");
            return;
        }

        if (initializationTimer) {
            clearTimeout(initializationTimer);
        }

        isInitializing = true;
        initializationTimer = setTimeout(() => {
            try {
                initializeSurvey(data);
            } finally {
                isInitializing = false;
            }
        }, DEBOUNCE_DELAY);
    });
});'

  # Combine all parts
  paste0(
    js_prefix,
    js_type_defs,
    js_cookie_functions,
    js_survey_functions,
    js_initialize_survey,
    js_handlers
  )
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
