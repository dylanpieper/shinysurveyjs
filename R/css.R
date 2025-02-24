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
#' @param custom_css String. Additional CSS rules to append to the theme.
#'   Default: NULL.
#'
#' @return String containing complete CSS stylesheet for survey styling
#'
#' @importFrom glue glue
#' @keywords internal
generate_survey_theme <- function(
    theme = "defaultV2",
    primary = "#003594",
    custom_css = NULL) {
  stopifnot(
    is.character(primary),
    theme %in% c("defaultV2", "modern")
  )

  primary_light <- adjust_hex(primary, 25)
  primary_dark <- adjust_hex(primary, -25)
  button_text_color <- get_contrast_color(primary)
  button_text_color_light <- get_contrast_color(primary_light)

  background <- "#ffffff"
  foreground <- "#404040"
  input_background <- "#ffffff"
  border_color <- "#e0e0e0"
  hover_background <- "#f5f5f5"
  header_background <- "#e7e7e7"
  disabled_color <- "#dbdbdb"
  text_border <- "#d4d4d4"
  container_background <- "#f9f9f9"

  base_css <- if (theme == "modern") {
    glue::glue("
.sv-root-modern {{
    --main-color: {primary};
    --main-hover-color: {primary_light};
    --text-color: {foreground};
    --light-text-color: {button_text_color};
    --disabled-text-color: rgba(64, 64, 64, 0.5);
    --foreground-light: #909090;
    --body-background-color: {background};
    --body-container-background-color: {hover_background};
    --header-background-color: {header_background};
    --answer-background-color: {sprintf('rgba(%s, 0.2)', paste(hex_to_rgb(primary), collapse = ', '))};
    --container-background: {container_background};
    --inputs-background-color: transparent;
    --text-border-color: {text_border};
    --border-color: {border_color};
    --add-button-color: {primary};
    --remove-button-color: #ff1800;
    --clean-button-color: {primary};
    --progress-text-color: #9d9d9d;
    --progress-buttons-color: {adjust_hex(primary, 40)};
    --disable-color: {disabled_color};
    --disabled-label-color: rgba(64, 64, 64, 0.5);
    --error-color: #d52901;
    --error-background-color: rgba(213, 41, 1, 0.2);
    --slider-color: white;
    --disabled-slider-color: #cfcfcf;
    --checkmark-color: {button_text_color};
    --radio-checked-color: {foreground};
}}")
  } else {
    glue::glue("
:root {{
    --primary: {primary};
    --primary-light: {primary_light};
    --primary-dark: {primary_dark};
    --primary-foreground: {button_text_color};
    --background: {background};
    --foreground: {foreground};
    --input-background: {input_background};
    --border-color: {border_color};
    --hover-background: {hover_background};
    --container-background: {container_background};
    --sd-button-primary-background: var(--primary);
    --sd-button-primary-text-color: {button_text_color};
    --sd-navigation-button-background: var(--primary);
    --sd-navigation-button-text-color: {button_text_color};
    --sd-navigation-button-hover: var(--primary-light);
}}

.sv-components-row {{
    border-top: 2px solid var(--primary) !important;
}}")
  }

  base_styles <- glue::glue("
body {{
    background-color: {background} !important;
    color: {foreground} !important;
    font-size: 16px !important;
}}

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
#surveyResponseContainer h3 {{
    color: var(--foreground) !important;
}}

.sv-body {{
    background-color: {container_background};
    color: {foreground};
    height: fill !important;
}}

.sd-body.sd-body--responsive {{
    height: -webkit-fill-available;
}}

.container-fluid {{
    padding-right: 0px !important;
    padding-left: 0px !important;
}}

.sv-components-column{{
    display: revert !important;
}}")

  container_styles <- glue::glue("
.sd-body.sd-completedpage,
.sv-body.sv-completedpage {{
    width: 100% !important;
    margin: 0 auto !important;
    padding: 2rem !important;
    box-sizing: border-box !important;
    height: -webkit-fill-available !important;
    display: flex !important;
    justify-content: center !important;
    align-items: center !important;
    min-height: 32vh !important;
    flex-direction: column-reverse;
    background-color: var(--container-background) !important;
    border-top: 2px solid var(--primary) !important;
}}

.sv-container-modern,
.sv-root-modern .sv-body {{
    border-radius: 50px;
    overflow: hidden;
}}

.sv-root-modern .sv-body.sv-completedpage {{
    height: auto !important;
    padding: 9rem !important;
    display: revert !important;
}}

.sd-completedpage__text {{
    text-align: center !important;
}}

.sv-root-modern,
.sd-root-modern {{
    width: 100% !important;
    overflow-x: hidden !important;
}}

.sv-container-modern,
.sv-body {{
    max-width: 1200px !important;
    margin: 0 auto !important;
    padding: 1rem !important;
    box-sizing: border-box !important;
    color: {foreground};
}}

.sv-question {{
    background-color: {container_background};
    border: 1px solid {border_color};
    border-radius: 4px;
    padding: 16px;
}}

.sv-question:hover {{
    background-color: {hover_background};
}}

.sv-question.sv-question--selected {{
    border-color: {primary};
    background-color: {hover_background};
}}

.sv-checkbox-material {{
    border-color: {border_color};
}}

.sv-footer.sv-action-bar {{
    background-color: {container_background};
    max-width: 1200px !important;
    margin: 0 auto !important;
}}")

  button_styles <- if (theme == "defaultV2") {
    glue::glue("
.sd-btn {{
    background-color: var(--primary) !important;
    color: {button_text_color} !important;
    border: none !important;
    transition: background-color 0.2s ease-in-out !important;
}}

.sd-btn:hover {{
    background-color: var(--primary-light) !important;
    color: {button_text_color} !important;
    opacity: 0.9;
}}

.sd-btn:disabled {{
    background-color: var(--border-color) !important;
    opacity: 0.6;
}}

.sd-navigation__next-btn,
.sd-navigation__prev-btn {{
    background-color: var(--primary) !important;
    color: {button_text_color} !important;
    border: none !important;
    transition: background-color 0.2s ease-in-out !important;
}}

.sd-navigation__next-btn:hover,
.sd-navigation__prev-btn:hover {{
    background-color: var(--primary-light) !important;
    color: {button_text_color} !important;
}}

.sd-btn--action[value=\"Complete\"] {{
    background-color: var(--primary) !important;
    color: {button_text_color} !important;
    border: none !important;
    transition: background-color 0.2s ease-in-out !important;
}}

.sd-btn--action[value=\"Complete\"]:hover {{
    background-color: var(--primary-light) !important;
    color: {button_text_color} !important;
}}

.sd-rating-item--selected {{
    background-color: var(--primary) !important;
    color: {button_text_color} !important;
}}")
  } else {
    glue::glue("
.sv-btn.sv-action-bar-item--secondary {{
    color: {button_text_color} !important;
}}

.sv-btn.sv-action-bar-item--secondary:hover {{
    background-color: {primary_light} !important;
    color: {button_text_color} !important;
    opacity: 0.9;
}}

.sv-footer__complete-btn,
.sv-btn--navigation {{
    color: {button_text_color} !important;
}}

.sv-footer__complete-btn:hover,
.sv-btn--navigation:hover {{
    background-color: {primary_light} !important;
    color: {button_text_color} !important;
    opacity: 0.9;
}}

.sv-root-modern .sv-btn,
.sv-root-modern .sv-footer__complete-btn,
.sv-root-modern .sv-nav-btn {{
    padding: 10px 25px !important;
    font-size: 2rem !important;
    min-height: 48px !important;
    line-height: 1.4 !important;
}}

.sv-root-modern .sv-footer__complete-btn {{
    font-weight: 600 !important;
}}")
  }

  message_css <- glue::glue("
.message-container {{
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    width: 90%;
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
    font-family: system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", sans-serif;
    font-size: 20px;
    color: {foreground};
    text-align: center;
    letter-spacing: -0.01em;
    transition: opacity 0.3s ease;
    padding: 24px;
    box-sizing: border-box;
    overflow-y: auto;
}}

@keyframes pulse {{
    0% {{
        transform: scale(0.8);
        opacity: 1;
    }}
    50% {{
        transform: scale(1.2);
        opacity: 0.2;
    }}
    100% {{
        transform: scale(0.8);
        opacity: 1;
    }}
}}

.loading-spinner {{
    width: 80px;
    height: 80px;
    position: relative;
    margin: 16px auto;
}}

.loading-spinner::before,
.loading-spinner::after {{
    content: \"\";
    position: absolute;
    inset: 0;
    border: 4px solid transparent;
    border-radius: 50%;
    animation: pulse 2s cubic-bezier(0.4, 0, 0.2, 1) infinite;
}}

.loading-spinner::before {{
    border-color: {primary};
}}

.loading-spinner::after {{
    border-color: {adjust_hex(primary, 20)};
    animation-delay: -1s;
}}

.loading-text {{
    position: relative;
    display: inline-block;
    font-size: 24px;
}}

.error-message {{
    font-size: 24px;
    font-weight: 600;
    color: {foreground};
    padding: 16px;
    border-radius: 8px;
    background: {container_background};
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
    width: 100%;
    word-wrap: break-word;
    overflow-wrap: break-word;
}}

@media screen and (max-width: 480px) {{
    .message-container {{
        padding: 12px;
        width: 95%;
        font-size: 16px;
    }}

    .loading-spinner {{
        width: 60px;
        height: 60px;
    }}

    .loading-spinner::before,
    .loading-spinner::after {{
        border-width: 3px;
    }}

    .loading-text {{
        font-size: 18px;
    }}

    .error-message {{
        font-size: 18px;
        padding: 12px;
    }}
}}")

  if (theme == "defaultV2") {
    additional_styles <- "
  .sd-footer {
    max-width: 850px !important;
    margin: 0 auto !important;
    padding: 2rem !important;
    box-sizing: border-box !important;
  }

  .sd-page {
    max-width: 850px !important;
    margin: 0 auto !important;
    padding: 0 2rem !important;
    box-sizing: border-box !important;
  }

  .sd-element--with-frame.sd-question--error-top {
    padding-top: 2rem;
  }
  "

    base_css <- paste(base_css, additional_styles, sep = "\n\n")
  }

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
