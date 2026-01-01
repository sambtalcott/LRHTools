# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

LRHTools is an internal R package for Littleton Regional Healthcare (LRH) providing utilities for:
- DuckDB database operations with encrypted database support
- OneDrive/Microsoft 365 file operations
- Tableau Cloud API integration
- ggplot2 theming with LRH brand colors
- Data import helpers for DA2 (Discern Analytics) CSV exports

## Development Commands

```bash
# Install package locally
R CMD INSTALL .

# Build and check package
R CMD build .
R CMD check LRHTools_*.tar.gz

# Run tests
Rscript -e "testthat::test_local()"

# Run single test file
Rscript -e "testthat::test_file('tests/testthat/test-duckdb.R')"

# Regenerate documentation
Rscript -e "devtools::document()"

# Load package for interactive testing
Rscript -e "devtools::load_all()"
```

## Architecture

### Package Environments
The package uses three internal environments to store state across function calls:
- `.ddb_env` (R/duckdb.R): Stores DuckDB connection, driver, database path, and connection type
- `.od_env` (R/od_.R): Stores the current OneDrive object (ms_drive or ms_drive_item)
- `.tab_env` (R/tableau.R): Stores Tableau authentication token and base request

### DuckDB Connection Management
- `lrh_con()` is the primary connection function; it returns existing connections when possible
- Connections are either "read_only", "read_write", or "any"
- Database file location is stored in Windows Credential Store under "LRH_DB" key
- Encrypted databases use DUCKDB_KEY credential via `tntpr::tntp_cred()`
- Timezone handling: imports use UTC, outputs convert to America/New_York
- Uses temp directory at C:/temp/duckdb_temp

### OneDrive Integration
- `od_default()` sets the working OneDrive/folder for subsequent `od_*` operations
- Supports three OneDrive object types: ms_drive (full drive), ms_folder (drive item folder), ms_item (file)
- Handles shared folders via `od_get_shared()` and shortcut folders via `od_get_shortcut()`
- File type detection in `od_read()`/`od_write()` uses extension-to-type mapping in `process_type()`

### SQL Helpers for dbplyr
Functions in R/sql.R return raw SQL strings for use with `!!` operator in dbplyr pipelines:
- `sql_flatten()` - STRING_AGG with ordering
- `sql_as_date()` - timezone-aware date casting
- `sql_ce_dt_tm()`, `sql_ce_date()` - parse DA2 date strings (format: '0:2025112718300000:0.000000:126:0')

## Key Patterns

### Credential Management
Uses `tntpr::tntp_cred()` for credential prompting and `keyring::key_get/key_set_with_value()` for Windows Credential Store.

### Date-Time Handling
- DuckDB writes use TIMESTAMPTZ via `write_tz_duckdb()`
- DA2 imports parse "dt_tm" columns with `lubridate::mdy_hm()`
- `lrh_csv()` preserves local timezone (unlike `readr::write_csv()` which converts to UTC)

### Error Messaging
Uses `cli` package for consistent error/warning/info messages with styled values (`{.val}`, `{.var}`, `{.code}`).
