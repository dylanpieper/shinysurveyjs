# shinysurveyjs<img src="man/figures/SSJS-Hex.svg" align="right" width="200" height="200"/>

The goal of this package is to integrate the flexible frontend of the [SurveyJS](https://surveyjs.io/) library with the reactive backend of [Shiny](https://shiny.posit.co/) to interface with a [PostgreSQL](https://www.postgresql.org/) database and create dynamic user experiences. Whether you need a simple feedback form or a complex survey system for your organization, this package is designed to scale with you.

## Basic Features

-   Host a single survey or multiple surveys in one app

-   Store and manage survey data in a [PostgreSQL](https://www.postgresql.org/) database, including metadata such as the duration to load and complete the survey, date created and updated, Shiny session ID, and IP address

-   Design surveys and create JSON objects with a user-friendly [visual editor](https://surveyjs.io/create-free-survey)

-   Save survey progress as cookies and resume later

-   Change the primary theme color and select from dark and light themes

## Advanced Features

-   Dynamically populate choices (i.e., response options) from a database table and create dependent inputs (e.g., select a car brand and filter available models)

-   Dynamically stage JSON objects in the database to modify surveys using a staging table

-   URL query tokens and one-time access tokens for secure survey distribution

-   Asynchronous worker to handle database updates without interrupting the survey, including app logging, updating staged JSON objects, and managing tokens

## Installation

``` r
# Install from GitHub
pak::pkg_install("dylanpieper/shinysurveyjs")
```

## Basic Usage

### Single Survey

Read the survey from a json object to host a single survey. Data is stored in a PostgreSQL database table hosted on [Supabase](https://supabase.com/).

``` r
library(shinysurveyjs)

survey <- '{
  "title": "R Package Feedback",
  "pages": [
    {
      "name": "feedback",
      "elements": [
        {
          "type": "rating",
          "name": "rating",
          "title": "Please rate the shinysurveyjs 📦:",
          "rateValues": [
            {"value": 1, "text": "⭐"},
            {"value": 2, "text": "⭐⭐"},
            {"value": 3, "text": "⭐⭐⭐"},
            {"value": 4, "text": "⭐⭐⭐⭐"},
            {"value": 5, "text": "⭐⭐⭐⭐⭐"}
          ],
          "rateMax": 5,
          "isRequired": true
        },
        {
          "type": "comment",
          "name": "feedback",
          "visibleIf": "{rating} notempty",
          "title": "Why did you rate it {rating} stars?",
          "rows": 2
        },
        {
          "type": "html",
          "name": "lowRatingMessage",
          "visibleIf": "{rating} <= 2",
          "html": "I am sorry you had a poor experience. Please reach me at <b>dylanpieper@gmail.com</b> so I can help improve your experience."
        }
      ]
    }
  ]
}'

survey_single(
  json = survey,
  show_response = TRUE,
  theme_color = "#00AD6E",
  theme_mode = "dark",
  shiny_config = list(
    host = "0.0.0.0",
    port = 3838
  ),
  db_config = list(
    host = Sys.getenv("HOST"),                # aws-0-us-east-2.pooler.supabase.com
    port = as.numeric(Sys.getenv("PORT")),    # 5432
    db_name = Sys.getenv("DB_NAME"),          # postgres
    user = Sys.getenv("USER"),                # username
    password = Sys.getenv("PASSWORD"),        # password
    write_table = Sys.getenv("WRITE_TABLE"),  # survey_package_feedback
    log_table = Sys.getenv("LOG_TABLE")       # survey_app_logs
  ),
  cookie_expiration_days = 3
)
```

By default, the database configuration looks for environmental variables (e.g., `Sys.getenv("PASSWORD")`) that can be loaded from an `.env` or `.yaml` file or a secrets manager. Using encrypted secrets is recommended for production environments.
