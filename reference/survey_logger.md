# Survey Logger Class

R6 Class providing dual logging functionality for Shiny survey
applications: console messaging and database logging of survey metadata.

## Details

The logger provides two distinct logging mechanisms:

**Console Logging (`log_message`)**:

- Immediate color-coded console output for app messages, warnings, and
  errors

- No database persistence - console only

- Supports custom zones/contexts for message categorization

**Database Logging (`log_entry`)**:

- Queued logging of survey metadata to database table

- Tracks survey completion metrics (timing, errors, SQL statements)

- Uses batch processing with automatic queue management

- Only logs database errors after survey is loaded (conditional logging)

Key Features:

- Dual-purpose logging: console messages + database survey metadata

- Uses main application connection for database operations

- Efficient batch database writes with automatic queue processing

- Immediate console feedback with color coding and zone support

- Database connection health monitoring

- SQL statement tracking for database error debugging

## Console Message Types

- `INFO`: Regular informational messages (green console output)

- `WARN`: Warning messages (yellow console output)

- `ERROR`: Error messages (red console output)

## Public fields

- `log_table`:

  Character string specifying the database table for logs

- `session_id`:

  Character string containing unique session identifier

- `table_name`:

  Character string identifying the current table

- `survey_loaded`:

  Logical flag indicating if a survey has been successfully loaded

- `suppress_logs`:

  Logical flag to suppress console output

- `queue`:

  data.frame containing queued messages for batch processing

- `last_sql`:

  Character string containing the last executed SQL statement

## Methods

### Public methods

- [`survey_logger$new()`](#method-survey_logger-new)

- [`survey_logger$update_table_name()`](#method-survey_logger-update_table_name)

- [`survey_logger$mark_survey_loaded()`](#method-survey_logger-mark_survey_loaded)

- [`survey_logger$update_last_sql()`](#method-survey_logger-update_last_sql)

- [`survey_logger$log_entry()`](#method-survey_logger-log_entry)

- [`survey_logger$log_message()`](#method-survey_logger-log_message)

- [`survey_logger$clone()`](#method-survey_logger-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new survey logger instance

#### Usage

    survey_logger$new(log_table, session_id, table_name, echo = TRUE)

#### Arguments

- `log_table`:

  Character string specifying logging table name

- `session_id`:

  Character string containing session identifier

- `table_name`:

  Character string identifying the table

- `echo`:

  Logical flag to display console output. Default: TRUE

------------------------------------------------------------------------

### Method `update_table_name()`

Update the table name for this logger instance

#### Usage

    survey_logger$update_table_name(table_name)

#### Arguments

- `table_name`:

  Character string identifying the new table name

#### Returns

Invisible NULL

------------------------------------------------------------------------

### Method `mark_survey_loaded()`

Mark the survey as loaded to enable error logging

#### Usage

    survey_logger$mark_survey_loaded()

#### Returns

Invisible NULL

------------------------------------------------------------------------

### Method `update_last_sql()`

Update the last SQL statement executed

#### Usage

    survey_logger$update_last_sql(sql)

#### Arguments

- `sql`:

  Character string containing the SQL statement

#### Returns

Invisible NULL

------------------------------------------------------------------------

### Method `log_entry()`

Queue a log entry for database insert (only for loaded surveys)

#### Usage

    survey_logger$log_entry(
      table_id,
      message = NULL,
      ip_address = NULL,
      duration_load = NULL,
      duration_complete = NULL,
      duration_save = NULL,
      sql = NULL,
      force_log = FALSE
    )

#### Arguments

- `table_id`:

  Integer ID from the table

- `message`:

  Character string containing error message (only for DB errors after
  survey loaded)

- `ip_address`:

  Character string containing client IP address

- `duration_load`:

  Numeric time spent loading (seconds)

- `duration_complete`:

  Numeric time spent completing survey (seconds)

- `duration_save`:

  Numeric time spent saving (seconds)

- `sql`:

  Character string containing the SQL that failed (only for errors)

- `force_log`:

  Logical flag to force logging even if survey not loaded (internal use)

#### Returns

Invisible NULL

------------------------------------------------------------------------

### Method `log_message()`

Helper method for simple console logging (no database)

#### Usage

    survey_logger$log_message(message, type = "INFO", zone = "DEFAULT")

#### Arguments

- `message`:

  Character string containing message to log

- `type`:

  Character string specifying message type (INFO/WARN/ERROR)

- `zone`:

  Character string specifying the logging zone/context (default:
  "DEFAULT")

#### Returns

Invisible NULL

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    survey_logger$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
