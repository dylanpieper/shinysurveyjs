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

The dynamic configuration is specified through the `dynamic_config` parameter in `survey_single()`. It accepts a list of configuration objects.

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

Configure your survey with dynamic dropdowns and dependent fields:

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
# URL: http://127.0.0.1:3838/?source=github

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

### Parameter Dependencies

The dynamic configuration can be combined to create complex field dependencies. For example, you can use a URL query parameter to set the package name and then use the choice to populate a radio button field for the versions released for that package.

``` r
# URL: http://127.0.0.1:3838/?package=shinysurveyjs

dynamic_config = list(
  list(
    group_type = "param",                    # Type of configuration
    table_name = "config_packages",          # Database table
    group_col = "package"                    # Column for grouping/lookup
  ),
  list(
    group_type = "choice",                   # Type of configuration
    table_name = "config_packages_versions", # Database table
    parent_table_name = "config_packages",   # Parent table for dependency
    parent_id_col = "package_id",            # Parent id column for dependency
    group_col = "version"                    # Column for grouping/lookup
  )
)
```

## Complete Example

You can also combine both choice and parameter configuration types. For example, you can select the package and version loaded from a database table and display the referral source from a URL parameter:

You will need to set up a PostgreSQL database with the following schema:

``` sql
-- Create the packages configuration table
CREATE TABLE config_packages (
    package_id SERIAL PRIMARY KEY,
    package VARCHAR(255) NOT NULL,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(package)
);

-- Create the package versions table with proper foreign key
CREATE TABLE config_packages_versions (
    version_id SERIAL PRIMARY KEY,
    package_id INTEGER NOT NULL,
    version VARCHAR(50) NOT NULL,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (package_id) REFERENCES config_packages(package_id),
    UNIQUE(package_id, version)
);

-- Create the source configuration table
CREATE TABLE config_source (
    id SERIAL PRIMARY KEY,
    source VARCHAR(50) NOT NULL UNIQUE,
    display_text VARCHAR(255) NOT NULL,
    date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert sample data for packages
INSERT INTO config_packages (package) VALUES
    ('batchLLM'),
    ('shinysurveyjs');

-- Insert sample data for package versions
INSERT INTO config_packages_versions (package_id, version)
SELECT p.package_id, v.version
FROM config_packages p
CROSS JOIN (
    VALUES 
        ('batchLLM', 'dev-github'),
        ('batchLLM', 'CRAN-0.1.0'),
        ('batchLLM', 'CRAN-0.2.0'),
        ('shinysurveyjs', 'dev-github'),
        ('shinysurveyjs', 'CRAN-0.1.0')
) AS v(package_name, version)
WHERE p.package = v.package_name;

-- Insert sample data for source configuration
INSERT INTO config_source (source, display_text) VALUES
    ('github', 'GitHub'),
    ('cran', 'CRAN'),
    ('posit', 'the Posit Forum');
```

You can then run the following code to create the survey:

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
