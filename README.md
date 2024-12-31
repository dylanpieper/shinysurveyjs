# shinysurveyjs<img src="man/figures/SSJS-Hex.svg" align="right" width="300" height="300"/>

The goal of this package is to integrate the flexible frontend of the [SurveyJS](https://surveyjs.io/) library with the reactive backend of [Shiny](https://shiny.posit.co/) to interface with a database and create dynamic user experiences. Whether you need a simple feedback form or a complex survey system for your organization, this package is designed to scale with you.

## Key Features

-   Host a single survey or multiple surveys in one app

-   Dynamically populate field choices (i.e., response options) from a database table and create dependent fields (e.g., select a car brand and filter available models)

-   URL query tokens and one-time access tokens for controlled survey distribution

-   Asynchronous worker to handle database updates in near real-time

## Installation

``` r
# Install from GitHub
pak::pkg_install("dylanpieper/shinysurveyjs")
```

## Basic Usage

### Single Survey

Read the survey from a json object to host a single survey. Data is stored in a PostgreSQL database table.

``` r
library(shinysurveyjs)

survey <- '{
  "title": "R Package Feedback",
  "pages": [
    {
      "name": "userInfo",
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
          "rateMax": 5
        },
        {
          "type": "comment",
          "name": "feedback",
          "visibleIf": "{rating} notempty",
          "title": "Why did you rate it {rating} stars?",
          "rows": 2
        }
      ]
    }
  ]
}'

survey_single(json = survey,
              theme_color = "#00AD6E",
              theme_mode = "dark")
```
