# shinysurveyjs<img src="man/figures/SSJS-Hex.svg" align="right" width="160" height="160"/>

Deploy survey applications using [SurveyJS](https://surveyjs.io) and [Shiny](https://shiny.posit.co) for R with advanced database integration.

## Overview

shinysurveyjs bridges the gap between SurveyJS's frontend for survey creation and Shiny's backend for data plumbing, enabling individuals and organizations to deploy survey applications with:

-   **Bring your own database**: Connect to your own database, and create your survey tables for full control
-   **Multisurvey applications**: Use URL-based routing to share multiple surveys in one application
-   **Many-to-one mapping**: Map multiple surveys to a single table
-   **Database logic for survey fields**: Populate field choices and validate fields from database sources
-   **Dual logging system**: Log operation messages to console for development, and log errors and metadata to database for production

## Installation

Install the development version from GitHub:

``` r
pak::pak("dylanpieper/shinysurveyjs")
```

## Quick start

This example shows a portion of two grant management surveys where researchers first drop opportunity information, then later create concept models for those opportunities. This pattern applies to many scenarios including patient intake and follow-up or event registration and feedback.

``` r
library(shinysurveyjs)

# Database configuration (reused across examples)
db_config <- list(
  host = Sys.getenv("DB_HOST"),
  port = as.numeric(Sys.getenv("DB_PORT")),
  name = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  pass = Sys.getenv("DB_PASS"),
  write_table = "survey_data",
  log_table = "sjs_logs",
  auth_table = "sjs_auth",
  pool_size = 10
)

# Define the first survey: Grant opportunity drop tracking
grant_drops <- list(
  title = "Opportunity Drop",
  pages = list(
    list(
      name = "drop",
      elements = list(
        list(
          type = "text",
          name = "title",
          title = "Official title of opportunity",
          description = "Please copy/paste to capture any potential duplicates",
          isRequired = TRUE
        ),
        list(
          type = "html",
          name = "match_warning",
          visible = FALSE,
          html = "🛑 <b>Error:</b> Duplicate submission(s) were found in the database."
        ),
        list(
          type = "dropdown",
          name = "grant_funders_id",
          title = "Funder",
          isRequired = TRUE,
          choices = c("Placeholder"), # Populated from database
          showOtherItem = TRUE,
          storeOthersAsComment = FALSE
        )
      )
    )
  )
)

# Define the second survey: Concept model creation
grant_concept <- list(
  title = "Concept Model",
  pages = list(
    list(
      name = "concept",
      elements = list(
        list(
          type = "dropdown",
          name = "grant_drops_id",
          title = "Select grant",
          isRequired = TRUE,
          choices = c("Placeholder") # Populated from previous survey data
        ),
        list(
          type = "text",
          name = "cm_note",
          title = "Note your concept for this grant",
          isRequired = TRUE
        )
      )
    )
  )
)

# Deploy multi-survey application
survey(
  list = list(
    "grant_drops" = grant_drops,     # Access at /?survey=grant_drops
    "grant_concept" = grant_concept  # Access at /?survey=grant_concept
  ),
  shiny_config = list(
    host = "0.0.0.0",
    port = 3838
  ),
  db_config = db_config,
  
  # Update grant_drops table when concept model is submitted
  db_update = list(
    list(
      from = "grant_concept",
      to = "grant_drops",
      by = c("grant_drops_id" = "id")
    )
  ),
  
  # Database field logic
  db_logic = list(
    # Populate funders dropdown from database
    list(
      type = "choice",
      source_tbl = "grant_funders",
      source_col = "id",
      source_display_col = c("name", "agency"), # Pastes column strings
      target_tbl = "grant_drops",
      target_col = "grant_funders_id"
    ),
    
    # Prevent duplicate grant titles
    list(
      type = "unique",
      source_tbl = "grant_drops",
      source_col = "title",
      target_tbl = "grant_drops",
      target_col = "title",
      result = "stop",
      result_field = "match_warning"
    ),
    
    # Populate available grants
    list(
      type = "choice",
      source_tbl = "grant_drops",
      source_col = "id",
      source_display_col = "title",
      target_tbl = "grant_concept",
      target_col = "grant_drops_id",
      filter_source = "is.na(cm_target_pop)" # Show grants without concept models
    )
  )
)
```

## Single survey usage

For simple single-survey applications, pass your survey definition directly:

``` r
library(shinysurveyjs)

# Example survey definition
feedback_survey <- list(
  title = "Feedback Survey",
  pages = list(
    list(
      name = "feedback",
      elements = list(
        list(
          type = "rating",
          name = "satisfaction",
          title = "How satisfied are you with our service?",
          rateMin = 1,
          rateMax = 5,
          isRequired = TRUE
        ),
        list(
          type = "comment",
          name = "comments",
          title = "Additional comments"
        )
      )
    )
  )
)

# Using JSON definition
survey(
  json = '{"title": "Quick Poll", "pages": [{"name": "poll", "elements": [{"type": "text", "name": "response", "title": "What do you think?", "isRequired": true}]}]}',
  db_config = db_config
)

# Using list definition  
survey(
  list = feedback_survey,
  db_config = db_config
)
```

Use `write_table` in `db_config` when deploying single surveys or unnamed survey lists. For named survey lists (multi-survey apps), table names are derived from the list names.

## Key features

### Database integration

-   **DBI database support**: Our team is testing this application with MySQL (default), but other DBI-compatible database drivers can be used.
-   **Connection pooling**: Automatic connection pooling with configurable pool size (default: 10 connections) for concurrent request handling.
-   **Transaction safety**: Full ACID compliance with automatic transaction management. Operations use dedicated connections from the pool with automatic commit/rollback on success/failure.

### Database logic for survey fields

Control survey behavior through database logic:

``` r
# Choice fields populated from database
list(
  type = "choice",
  source_tbl = "categories",       # Data source table  
  source_col = "id",               # Value column
  source_display_col = "name",     # Display text column
  target_col = "category_id",      # Survey field to populate
  filter_source = "active == 1",   # R expression filter
  filter_unique = TRUE             # Remove already-used options
)

# Parameter validation from URLs  
list(
  type = "param",
  source_tbl = "valid_sources",
  target_col = "referral_source"   # Validates and stores in hidden field
)

# Uniqueness validation
list(
  type = "unique", 
  source_tbl = "participants",
  source_col = "email",
  result = "warn",                 # "warn" or "stop"
  result_field = "email_warning"   # Field to show warning message
)
```

### Dual logging system

-   **Console logging**: Immediate feedback during development
-   **Database logging**: Production monitoring and metadata
    -   Error logging with SQL statement capture
    -   Survey completion timing (load, complete, and save durations)
    -   IP address tracking

**The database log table will be created automatically.**

### Advanced configuration

``` r
# Database configuration
db_config <- list(
  host = Sys.getenv("DB_HOST"),
  port = as.numeric(Sys.getenv("DB_PORT")),
  name = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  pass = Sys.getenv("DB_PASS"),
  write_table = "survey_data",
  log_table = "sjs_logs",
  auth_table = "sjs_auth",
  pool_size = 10
)

survey(
  list = my_surveys,
  db_config = db_config,
  
  # Shiny server configuration
  shiny_config = list(
    host = "0.0.0.0",      # Bind to all interfaces
    port = 8080            # Custom port
  ),
  
  # Theme customization
  theme = "modern",        # "defaultV2" or "modern"
  theme_color = "#1f77b4", # Primary color
  custom_css = "
    .sv-root { font-family: 'Roboto', sans-serif; }
    .sv-page { max-width: 800px; margin: 0 auto; }
  ",
  
  # Progress tracking
  cookie_expiration_days = 30,  # Save progress for 30 days
  
  # Development/production options
  show_response = TRUE,    # Display data table after submission
  echo = FALSE             # Hide console output
)
```

#### Environment variables

Set database connection parameters using environment variables:

``` r
# .Renviron file
DB_HOST=database.example.com
DB_PORT=3306 
DB_NAME=research_db
DB_USER=db_user
DB_PASS=db_pass
```

#### Secure credential management

Use [keyring](https://keyring.r-lib.org/) for production deployments:

``` r
library(keyring)

# Store credentials securely
keyring::key_set("db_pass", username = "research_db")

# Database configuration with secure credentials
db_config <- list(
  host = Sys.getenv("DB_HOST"),
  port = as.numeric(Sys.getenv("DB_PORT")),
  name = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  pass = keyring::key_get("db_pass", "research_db"),
  write_table = "survey_data",
  log_table = "sjs_logs",
  auth_table = "sjs_auth",
  pool_size = 10
)

# Use in survey
survey(list = my_surveys, db_config = db_config)
```
