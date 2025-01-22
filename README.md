# shinysurveyjs![A hex logo for an R package called shinysurveyjs](man/figures/SSJS-Hex.svg){align="right" width="200" height="200"}

The goal of this package is to integrate the flexible frontend of the [SurveyJS](https://surveyjs.io/) library with the reactive backend of [Shiny](https://shiny.posit.co/) to interface with a [PostgreSQL](https://www.postgresql.org/) database and create dynamic user experiences. Whether you need a simple feedback form or a complex survey system for your organization, this package is designed to scale with you.

## SurveyJS

SurveyJS is a JavaScript library that streamlines the creation of survey applications through a [jQuery](https://www.npmjs.com/package/survey-jquery) architecture. The library offers a [visual editor](https://surveyjs.io/create-free-survey) that allows developers to design complex surveys through a drag-and-drop interface and generate a JSON object.

The JSON defines every survey element, including survey titles, descriptions, multi-page layouts, progress indicators, over 20 different question types, input validation rules, conditional logic flows, and field visibility controls.

The library's strength lies in its backend-agnostic approach, supporting seamless integration with various server technologies, while its client-side implementation handles advanced features like text piping with minimal code.

For R applications, developers can easily incorporate SurveyJS by parsing the JSON either as a raw text string or by converting an R list to JSON format.

## Basic Features

-   Host a single survey in one app (multiple surveys coming soon...)

-   Store data in a PostgreSQL database, including metadata such as the duration to load, complete, and save the survey; date created and updated; Shiny session ID; and IP address

-   Change the primary theme color and select from light to dark themes

-   Automatically save survey progress as cookies and resume later

## Advanced Features

-   Dynamically populate field choices (i.e., response options) from a database table and create dependent inputs (e.g., select a package name and filter available versions) with support for tracking via URL parameters (e.g., referral source; see [vignette](articles/dynamic_field_config.html))

-   Log app messages, warnings, and errors

-   Setup asynchronous future plan to update database without interrupting the survey

## Installation

``` r
pak::pkg_install("dylanpieper/shinysurveyjs")
```

## Basic Usage

Imagine I want to develop a survey for my `shinysurveyjs` package for users to rate it let me know how they feel about my work. I can use the following code to design and deploy this idea. I only need to have a PostgreSQL database and a Shiny server to run the app. The package automatically creates and updates the survey and app log tables.

### Single Survey

Define the survey configuration from a **JSON** text string or **list** to host a single survey. Data is stored in a PostgreSQL database table hosted on [Supabase](https://supabase.com/).

#### JSON

``` r
survey <- '{
  "title": "R Package Feedback",
  "pages": [
    {
      "name": "feedback",
      "elements": [
        list(
          type = "rating",
          name = "rating",
          title = "Please rate the shinysurveyjs 📦:",
          rateType = "stars",
          rateMax = 5,
          isRequired = TRUE
        ),
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
          "html": "I am sorry you had a poor experience. Please reach me at <b>dylanpieper@gmail.com</b> so I can help make it right."
        }
      ]
    }
  ]
}'

shinysurveyjs::survey_single(
  json = survey,
  theme = "modern",
  theme_color = "#00AD6E",
  theme_mode = "dark",
  db_config = list(
    host = Sys.getenv("HOST"),                # aws-0-us-east-2.pooler.supabase.com
    port = as.numeric(Sys.getenv("PORT")),    # 5432
    db_name = Sys.getenv("DB_NAME"),          # postgres
    user = Sys.getenv("USER"),                # username
    password = Sys.getenv("PASSWORD"),        # password
    write_table = Sys.getenv("WRITE_TABLE"),  # survey_package_feedback
    log_table = Sys.getenv("LOG_TABLE")       # survey_app_logs
  )
)
```

#### List

``` r
survey <- list(
  title = "R Package Feedback",
  pages = list(
    list(
      name = "feedback",
      elements = list(
        list(
          type = "rating",
          name = "rating",
          title = "Please rate the shinysurveyjs 📦:",
          rateType = "stars",
          rateMax = 5,
          isRequired = TRUE
        ),
        list(
          type = "comment",
          name = "feedback",
          visibleIf = "{rating} notempty",
          title = "Why did you rate it {rating} stars?",
          rows = 2
        ),
        list(
          type = "html",
          name = "lowRatingMessage",
          visibleIf = "{rating} <= 2",
          html = "I am sorry you had a poor experience. Please reach me at <b>dylanpieper@gmail.com</b> so I can make it right."
        )
      )
    )
  )
)

shinysurveyjs::survey_single(
  list = survey,
  ...
)
```

By default, the database configuration looks for environmental variables (e.g., `Sys.getenv("PASSWORD")`) that can be loaded from an `.env` or `.yaml` file or a secrets manager. Using encrypted secrets is recommended for production environments.

## Roadmap

-   Add survey start and end date controls to limit access to a survey

-   Enable one-time access tokens for secure survey distribution (piggyback on the dynamic field config for URL parameters to turn off access after completion)

-   Dynamically stage JSON objects in the database to modify surveys using a staging table

-   Add mode for URL parameter encryption

-   Add to asynchronous worker: Update staged JSON objects and manage tokens

-   Support multiple surveys with JSON loaded from database (`survey_multi()`)
