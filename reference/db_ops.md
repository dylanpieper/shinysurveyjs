# Database Operations Class

R6 Class for managing database operations related to survey data storage
and retrieval using MySQL and other DBI-compatible databases.

## Details

This class handles all database interactions for survey data, including:

- Table creation and modification

- Data insertion and retrieval

- Transaction management

- Error handling and logging

## Public fields

- `conn`:

  Database connection

- `logger`:

  Logger instance for tracking operations

## Methods

### Public methods

- [`db_ops$new()`](#method-Database%20Operations-new)

- [`db_ops$operate()`](#method-Database%20Operations-operate)

- [`db_ops$create_survey_table()`](#method-Database%20Operations-create_survey_table)

- [`db_ops$update_survey_table()`](#method-Database%20Operations-update_survey_table)

- [`db_ops$read_table()`](#method-Database%20Operations-read_table)

- [`db_ops$update_by_id()`](#method-Database%20Operations-update_by_id)

- [`db_ops$perform_survey_update()`](#method-Database%20Operations-perform_survey_update)

- [`db_ops$get_required_columns()`](#method-Database%20Operations-get_required_columns)

- [`db_ops$clone()`](#method-Database%20Operations-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Database Operations instance

#### Usage

    db_ops$new(conn, logger)

#### Arguments

- `conn`:

  Connection pool object. Database connection pool

- `logger`:

  survey_logger object. Logger instance for tracking operations

------------------------------------------------------------------------

### Method `operate()`

Execute a database operation with transaction handling using connection
pool

#### Usage

    db_ops$operate(operation, error_message)

#### Arguments

- `operation`:

  Function. The database operation to execute

- `error_message`:

  Character. Message to display if operation fails

#### Returns

Result of the operation or error message if failed

------------------------------------------------------------------------

### Method `create_survey_table()`

Check pre-defined survey table for required columns

#### Usage

    db_ops$create_survey_table(write_table, data, survey_obj = NULL)

#### Arguments

- `write_table`:

  Character. Name of the pre-defined table to check

- `data`:

  data.frame. Data frame containing the required columns for the survey

- `survey_obj`:

  Survey.JS definition object that contains the complete survey
  structure. A nested list containing:

  pages

  :   List of survey pages

  elements

  :   List of survey questions/elements, where each element contains:

  name

  :   Question identifier

  type

  :   Question type (e.g., "checkbox", "text", "radio")

  showOtherItem

  :   Logical. Whether the question has an "other" option

  Used to determine column types and handle special question features
  like "other" options. Default: NULL

#### Returns

Character. The sanitized table name Update existing survey table with
new data

------------------------------------------------------------------------

### Method `update_survey_table()`

#### Usage

    db_ops$update_survey_table(write_table, data)

#### Arguments

- `write_table`:

  Character. Name of the table to update

- `data`:

  data.frame. Data frame containing the new data

#### Returns

Character. The sanitized table name

------------------------------------------------------------------------

### Method `read_table()`

Read data from a survey table with optional filtering

#### Usage

    db_ops$read_table(
      table_name,
      columns = NULL,
      filters = NULL,
      order_by = NULL,
      desc = FALSE,
      limit = NULL,
      update_last_sql = TRUE
    )

#### Arguments

- `table_name`:

  Character. Name of the table to read from

- `columns`:

  Character vector. Specific columns to read (NULL for all columns)

- `filters`:

  List. Named list of filter conditions (e.g., list(status = "active"))

- `order_by`:

  Character vector. Columns to order by

- `desc`:

  Logical. If TRUE, sort in descending order

- `limit`:

  Numeric. Maximum number of rows to return (NULL for all rows)

- `update_last_sql`:

  Logical. If TRUE, update logger's last_sql_statement (default: TRUE)

#### Returns

data.frame. The requested data

------------------------------------------------------------------------

### Method `update_by_id()`

Update specific columns in a table for a given row ID

#### Usage

    db_ops$update_by_id(table_name, id, values)

#### Arguments

- `table_name`:

  Character. Name of the table to update

- `id`:

  Numeric. Row ID to update

- `values`:

  List. Named list of column-value pairs to update

#### Returns

Invisible(NULL)

------------------------------------------------------------------------

### Method `perform_survey_update()`

Perform survey update operation using join columns

#### Usage

    db_ops$perform_survey_update(source_data, target_table, join_columns)

#### Arguments

- `source_data`:

  data.frame. Data from the source survey to use for updates

- `target_table`:

  Character. Name of the target table to update

- `join_columns`:

  Named character vector. Columns to join on (source_col = target_col)

#### Returns

Invisible(NULL)

------------------------------------------------------------------------

### Method `get_required_columns()`

Get required columns for a survey to help with pre-defined table setup

#### Usage

    db_ops$get_required_columns(data, survey_obj = NULL)

#### Arguments

- `data`:

  data.frame. Data frame containing the survey data structure

- `survey_obj`:

  Survey.JS definition object that contains the complete survey
  structure. Used to identify fields with "other" options that need
  separate columns.

#### Returns

List containing required column names and their recommended MySQL types

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    db_ops$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
