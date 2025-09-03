# shinysurveyjs (development version)

## Breaking changes

- **Parameter name changes**: For consistency, renamed function parameters:
  - `survey_auth` → `sjs_auth` (now exposed as direct parameter in `survey()`)
  - `survey_log` → `sjs_logs` (now exposed as direct parameter in `survey()`)
  
- **Database table field renaming**: To improve clarity and consistency:
  - `survey_name` → `table_name` (in logging table)
  - `survey_id` → `table_id` (in logging table)  
  - `sql_statement` → `sql` (in logging table)

## Improvements

- Authentication table (`sjs_auth`) and logging table (`sjs_logs`) can now be customized directly via `survey()` parameters
- For surveys using `db_update`, the logging table now correctly records the target table name being updated to (rather than the source survey name)
- Updated roxygen documentation to reflect parameter name changes