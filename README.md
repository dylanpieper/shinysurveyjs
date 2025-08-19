# shinysurveyjs ![](man/figures/ssjs-hex.svg){align="right" width="150" height="150"}

Deploy [SurveyJS](https://surveyjs.io) survey applications using [Shiny](https://shiny.posit.co) for R to interface with databases and create custom user experiences.

## SurveyJS

SurveyJS is a JavaScript library that streamlines the creation of survey applications through a [jQuery](https://www.npmjs.com/package/survey-jquery) architecture. The library offers a [visual editor](https://surveyjs.io/create-free-survey) to design complex surveys through a drag-and-drop interface and generate a JSON object.

The JSON defines every survey element, including a title, description, logo, and page layout as well as progress indicators, question types, validation rules, logic flows, visibility controls, and text piping. The library's strength lies in its backend-agnostic approach, supporting integration with any server technology.

In R & Shiny applications, developers can parse the JSON either as a raw text string or by converting a list to JSON format, which then gets passed to the front-end. LLMs are great for converting a JSON object to a list.

## Basic Features

-   Store data in a PostgreSQL database, including timing metadata (duration to load, duration to complete, duration to save, date created, and date updated) and tracking metadata (Shiny session ID and IP address)
-   All of the database tables that the app writes to are automatically created if they don't already exist
-   Automatically save survey progress as cookies and resume later
-   Change the theme, primary color, and contrast mode (light or dark)

## Advanced Features

-   Dynamically populate field choices (i.e., response options) from a database table and create dependent inputs (e.g., select a package name and filter available versions) with support for tracking via URL parameters (e.g., referral source); see [Database Logic Configuration](https://dylanpieper.github.io/shinysurveyjs/articles/db_logic_config.html)
-   Log app messages, warnings, and errors in a database table

## Installation

``` r
pak::pak("dylanpieper/shinysurveyjs")
```

## Basic Usage

Imagine I want to develop a feedback survey for my package.

### Single Survey

Let's define the survey as a list and then run the `survey()` function. I'm using [Supabase](https://supabase.com/) for my PostgreSQL database.

#### JSON

``` r
survey <- list(
  title = "R Package Feedback",
  pages = list(
    list(
      name = "page1",
      elements = list(
        list(
          type = "matrix",
          name = "rating",
          title = "Please rate the shinysurveyjs 📦:",
          isRequired = TRUE,
          columns = list(
            list(value = "1", text = "Very Bad"),
            list(value = "2", text = "Bad"),
            list(value = "3", text = "Neutral"),
            list(value = "4", text = "Good"),
            list(value = "5", text = "Very Good")
          ),
          rows = list(
            list(value = "UI", text = "UI Design"),
            list(value = "Server", text = "Server Functionality")
          )
        ),
        list(
          type = "comment",
          name = "feedback",
          title = "Any other feedback?",
          isRequired = TRUE
        )
      )
    )
  )
)

shinysurveyjs::survey(
  list = survey,
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

By default, the database configuration looks for environmental variables (e.g., `Sys.getenv("PASSWORD")`) that can be loaded from the `.Renviron` or your preferred method for handling environmental variables (e.g., [keyring](https://keyring.r-lib.org)).

See a more complex [gist of this form with expressions and text piping](https://gist.github.com/dylanpieper/c570dba08f03daa25445dfe5aea9ab15).
