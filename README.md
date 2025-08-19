# shinysurveyjs ![shinysurveyjs website](man/figures/ssjs-hex.svg){alt="shinysurveyjs website" align="right" width="130"}

Deploy survey applications using [SurveyJS](https://surveyjs.io) and [Shiny](https://shiny.posit.co) for R with advanced database integration.

## Overview

shinysurveyjs bridges the gap between SurveyJS's powerful frontend capabilities and R's robust data ecosystem, enabling researchers and organizations to deploy survey applications with:

-   **Bring Your Own Tables**: Setup your own database and tables
-   **Multisurvey applications**: URL-based routing to share multiple surveys in one application
-   **Many-to-one mapping**: Map multiple surveys to a single table
-   **Database logic for survey fields**: Choice population and validation from database sources
-   **Dual logging system**: Console logging for development, and database logging for production
-   **Enterprise features**: Custom themes, simple tracking, and metadata collection

## Installation

Install the development version from GitHub:

``` r
pak::pak("dylanpieper/shinysurveyjs")
```

## Quick start

This example shows a portion of a grant management workflow where researchers first drop opportunity information, then later create concept models for those opportunities. This pattern applies to many research scenarios including patient intake and follow-up or event registration and feedback.

``` r
library(shinysurveyjs)

# Define the first survey: Grant opportunity tracking
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
          html = "⚠️ <b>Warning:</b> Duplicate submission(s) were found in the database."
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
  db_config = list(
    driver = RMariaDB::MariaDB(),
    host = "database.example.com",
    port = 3306,
    db_name = "research_db",
    user = "db_user",
    password = keyring::key_get("db_pass", "research_db"),
    log_table = "survey_logs"
  ),
  
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
      source_display_col = "name",
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
    
    # Populate available grants (only those without concept models)
    list(
      type = "choice",
      source_tbl = "grant_drops",
      source_col = "id",
      source_display_col = "title",
      target_tbl = "grant_concept",
      target_col = "grant_drops_id",
      filter_source = "is.na(cm_note)" # Only show grants without concept models
    )
  )
)
```

## Key features

### Database integration

-   **DBI database support**: Our team is testing this application with MySQL (default), but other DBI-compatible database drivers can be used.
-   **Transaction safety**: Atomic operations with rollback on errors, immediate non-blocking insertion for survey data, and queued data insertion for metadata logging.

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

-   **Database logging**: Production monitoring and analytics

    -   Error logging with SQL statement capture
    -   Survey completion timing (load, complete, and save durations)
    -   IP address tracking

### Advanced configuration

``` r
survey(
  list = my_surveys,
  
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

## Configuration

### Environment variables

Set database connection parameters using environment variables:

``` r
# .Renviron file
DB_HOST=database.example.com
DB_PORT=3306 
DB_NAME=research_db
DB_USER=db_user
DB_PASSWORD=db_pass
WRITE_TABLE=survey_responses # For single survey use case
LOG_TABLE=survey_logs
```

### Secure credential management

Use [keyring](https://keyring.r-lib.org/) for production deployments:

``` r
library(keyring)

# Store credentials securely
keyring::key_set("db_pass", username = "research_db")

# Use in survey configuration
db_config <- list(
  driver = RMariaDB::MariaDB(),
  host = "database.example.com",
  port = 3306,
  user = "db_user", 
  password = keyring::key_get("db_pass", "research_db"),
  db_name = "research_db",
  log_table = "survey_logs"
)
```