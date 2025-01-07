---
title: "Dynamic Field Configuration"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Dynamic Field Configuration}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

The package provides powerful dynamic field configuration capabilities that allow survey fields to populate their choices or text from database tables. This vignette explains how to use these features.

## Dynamic Configuration Types

The package supports two main types of dynamic configurations:

1.  **Choice Configuration**: Populates input choices (radio button and dropdown fields) with options from database tables
2.  **Parameter Configuration**: Sets field values from URL parameters or database lookups

### Basic Structure

The dynamic configuration is specified through the `dynamic_config` parameter in `survey_single()`. It accepts a list of configuration objects:

``` r
dynamic_config = list(
  list(
    group_type = "choice",                   # Type of configuration
    table_name = "config_packages",          # Database table
    group_col = "package"                    # Column for grouping/lookup
  ),
  list(
    group_type = "choice",                   # Type of configuration
    table_name = "config_packages_versions", # Database table
    parent_table_name = "config_packages",   # Parent table for dependency
    parent_id_col = "package_id",            # Parent id column for dependency
    group_col = "version"                    # Column for grouping/lookup
  ),
  list(
    table_name = "config_source",            # Database table
    group_type = "param",                    # Type of configuration
    group_col = "source",                    # URL parameter name
    display_col = "display_text"             # Optional: Show display text
  )
)
```

## Choice Configuration

Choice configuration allows you to populate dropdown or select fields with options from your database.

### Example: Car Selection

Consider this database structure:

``` sql
CREATE TABLE car_brands (
  brand_id SERIAL PRIMARY KEY,
  brand_name VARCHAR(100)
);

CREATE TABLE car_models (
  model_id SERIAL PRIMARY KEY,
  brand_id INTEGER REFERENCES car_brands(brand_id),
  model_name VARCHAR(100)
);
```

Configure your survey with dynamic dropdowns:

``` r
dynamic_config = list(
  list(
    group_type = "choice",
    table_name = "car_brands",
    group_col = "brand_name"
  ),
  list(
    group_type = "choice",
    table_name = "car_models",
    parent_table_name = "car_brands",
    parent_id_col = "brand_id",
    group_col = "model_name",
  )
)
```

## Parameter Configuration

Parameter configuration allows you to:

-   Accept values from URL parameters

-   Store values in hidden fields

-   Look up display text from database tables

### URL Parameter Example

``` r
# URL: http://survey.com/?source=github

dynamic_config = list(
  list(
    table_name = "config_source",
    group_type = "param",
    group_col = "source",              # Matches URL parameter name
    display_col = "display_text"       # Optional: Show display text
  )
)
```

This feature is useful for tracking individuals, groups, or referral sources. It also allows you to pipe data into the survey from other systems.

## Complete Example

Here's a full example using the R package feedback form to combine both configuration types. You can select the package and version loaded from a database table and display the referral source from a URL parameter:

``` r
library(shinysurveyjs)

survey <- '{
  "title": "R Package Feedback",
  "pages": [
    {
      "name": "feedback",
      "elements": [
        {
          "type": "html",
          "visibleIf": "{source} notempty",
          "html": "Hi, you have been referred to this survey from <b>{source}</b>. <br>Thank you for rating my R package! - Dylan"
        },
        {
          "type": "radiogroup",
          "name": "package",
          "title": "Select an R package:",
          "isRequired": true,
          "choices": []
        },
        {
          "type": "radiogroup",
          "name": "version",
          "title": "Select a version:",
          "isRequired": true,
          "visibleIf": "{package} notempty",
          "choices": []
        },
        {
          "type": "rating",
          "name": "rating",
          "title": "Please rate the {package} 📦:",
          "visibleIf": "{version} notempty",
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
        },
        {
          "type": "text",
          "name": "source",
          "visible": false
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
    host = Sys.getenv("HOST"),
    port = as.numeric(Sys.getenv("PORT")),
    db_name = Sys.getenv("DB_NAME"),
    user = Sys.getenv("USER"),
    password = Sys.getenv("PASSWORD"),
    write_table = Sys.getenv("WRITE_TABLE"),
    log_table = Sys.getenv("LOG_TABLE")
  ),
  dynamic_config = list(
    list(
      group_type = "choice",
      table_name = "config_packages",
      group_col = "package"
    ),
    list(
      group_type = "choice",
      table_name = "config_packages_versions", 
      parent_table_name = "config_packages",
      parent_id_col = "package_id",
      group_col = "version"
    ),
    list(
      table_name = "config_source",
      group_type = "param",
      group_col = "source",
      display_col = "display_text"
    )
  )
)
```
