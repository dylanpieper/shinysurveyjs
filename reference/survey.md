# Deploy a Survey Shiny Application

Creates and deploys a Shiny application for SurveyJS surveys with
database integration, dual logging system, and advanced dynamic
configuration. Supports both single surveys and multi-survey
applications with URL-based routing.

## Usage

``` r
survey(
  json = NULL,
  list = NULL,
  show_response = FALSE,
  theme = "defaultV2",
  theme_color = "#003594",
  shiny_config = list(host = "0.0.0.0", port = 3838),
  ldap_config = NULL,
  db_config = list(host = Sys.getenv("DB_HOST"), port =
    as.numeric(Sys.getenv("DB_PORT")), name = Sys.getenv("DB_NAME"), user =
    Sys.getenv("DB_USER"), pass = Sys.getenv("DB_PASS"), write_table = "survey_data",
    log_table = "sjs_logs", auth_table = "sjs_auth", pool_size = 10),
  db_update = NULL,
  db_logic = NULL,
  sjs_auth = "sjs_auth",
  sjs_logs = "sjs_logs",
  cookie_expiration_days = 0,
  custom_css = NULL,
  echo = TRUE
)
```

## Arguments

- json:

  String. JSON survey definition. Use for single survey applications.

- list:

  List. Survey structure(s). Can be:

  - Single survey: List with survey elements (converted to JSON)

  - Multi-survey: Named list where each element is a complete survey
    (e.g., `list("survey_1" = survey1, "survey_2" = survey2)`)

- show_response:

  Logical. Display responses in a data.table after submission. Default:
  `FALSE`.

- theme:

  String. SurveyJS theme: "defaultV2" or "modern". Default: "defaultV2".

- theme_color:

  String. Hex color code for primary theme customization. Default:
  "#003594".

- shiny_config:

  List. Shiny server configuration:

  - `host`: Server host address. Default: "0.0.0.0"

  - `port`: Server port number. Default: 3838

- ldap_config:

  List. Optional LDAP authentication configuration:

  - `host`: LDAP server hostname

  - `base_dn`: Base Distinguished Name for LDAP searches

  - `port`: LDAP port (default: 389)

  - `user_attr`: User attribute for authentication (default: "uid")

  - `domain`: Domain for UPN binding (e.g., "pitt.edu") - for Active
    Directory

  - `ssh_tunnel`: Local port number for SSH tunnel (assumes tunnel
    already running, e.g., `ssh_tunnel = 3389`)

  - `logo`: URL to logo image to display instead of title text
    (optional)

- db_config:

  List. Database connection parameters:

  - `host`: Database host (default: `Sys.getenv("DB_HOST")`)

  - `port`: Database port (default: `as.numeric(Sys.getenv("DB_PORT"))`)

  - `name`: Database name (default: `Sys.getenv("DB_NAME")`)

  - `user`: Database username (default: `Sys.getenv("DB_USER")`)

  - `pass`: Database password (default: `Sys.getenv("DB_PASS")`)

  - `write_table`: Survey data table (default: "survey_data")

  - `log_table`: Application logs table (default: "sjs_logs")

  - `auth_table`: Authentication sessions table (default: "sjs_auth")

  - `pool_size`: Maximum connections in pool (default: 10)

- db_update:

  List. Update configuration for multi-survey workflows. Each element
  contains:

  - `from`: Source survey name to update from

  - `to`: Target survey/table name to update

  - `by`: Named vector for join columns (e.g.,
    `c("source_id" = "target_id")`)

- db_logic:

  List. Dynamic database configurations supporting multiple types:

  ### Choice Configuration (`type = "choice"`)

  Populates dropdown/radio choices from database tables: \*
  `source_tbl`: Database table containing choices \* `source_col`:
  Column with choice values \* `source_display_col`: Optional column for
  display text (defaults to `source_col`) \* `target_tbl`: Target
  survey/table name (for multi-survey filtering) \* `target_col`: Survey
  field to populate \* `filter_source`: R expression to filter source
  data (e.g., `"is.na(status)"`) \* `filter_unique`: Logical. Remove
  choices already used in target table

  ### Parameter Configuration (`type = "param"`)

  Validates URL query parameters against database values: \*
  `source_tbl`: Database table with valid parameter values \*
  `target_col`: URL parameter name to validate

  ### Uniqueness Validation (`type = "unique"`)

  Prevents duplicate entries in database fields: \* `source_tbl`:
  Database table to check against \* `source_col`: Database column to
  check for duplicates \* `target_tbl`: Target survey/table name \*
  `target_col`: Survey field to validate \* `result`: Action on
  duplicate - "warn" or "stop" \* `result_field`: Survey field for
  warning display (use hidden HTML element)

- sjs_auth:

  String. Name of authentication sessions table (default: "sjs_auth").

- sjs_logs:

  String. Name of application logs table (default: "sjs_logs").

- cookie_expiration_days:

  Numeric. Days to retain survey progress cookies. Default: 0 (no
  cookies).

- custom_css:

  String. Additional CSS rules to append to the theme.

- echo:

  Logical. Display console logging messages. Default: `TRUE`.

## Value

A Shiny application object

## Details

The dual logging system provides:

- Console logging: Immediate zoned messages for development and
  monitoring

- Database logging: Survey metadata including timing, IP addresses, and
  error tracking

## Examples

``` r
if (FALSE) { # \dontrun{
# Single survey with basic configuration
survey <- list(
  title = "Feedback Survey",
  pages = list(
    list(
      name = "feedback",
      elements = list(
        list(
          type = "radiogroup",
          name = "rating",
          title = "How satisfied are you?",
          choices = c("Very satisfied", "Satisfied", "Neutral", "Dissatisfied")
        )
      )
    )
  )
)

survey(
  list = survey,
  db_config = list(
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
)
} # }
```
