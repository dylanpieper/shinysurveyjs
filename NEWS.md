# shinysurveyjs (development version)

## New Features

- **Multisurvey Support**: Multiple surveys can now be deployed in a single application with automatic landing page generation
- **Database Logic Choice Filtering**: Added `filter` type for database logic configurations with SQL-based filtering and duplicate prevention
- **Enhanced Security**: Cookies (experimental) are now disabled by default

## API Changes

- Updated `db_logic` parameter structure with cleaner naming:
  - `type`: Replaces `config_type`
  - `source_tbl`: Replaces `table_name`
  - `source_col`: Rreplaces `config_col`
  - `target_tbl`: Survey name for multisurvey filtering
  - `target_col`: Column for joins when names differ

## Infrastructure

- Added dedicated multisurvey server logic and UI components
- Enhanced CSS theming for landing pages
- Improved logging and error handling
- Database table names automatically use survey names in multisurvey mode

## Backwards Compatibility

- Single survey applications continue to work unchanged
- Existing configurations remain supported