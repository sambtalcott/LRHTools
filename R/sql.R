
# Helper to quote column names for SQL (handles spaces and special characters)
#
# `x` is normally an expression captured with enexpr(), so it can be a symbol
# (bare column name), a string, an already-built `sql()` object, or a *call*
# such as `sql("NOW()")`. Calls must be evaluated in the caller's environment
# first: as.character() on a call returns its parts ("sql", "NOW()"), which
# used to silently collapse to the function name.
quote_col <- function(x, env = rlang::caller_env()) {
  if (inherits(x, "sql")) return(x)

  if (rlang::is_call(x)) {
    x <- eval(x, env)
    if (inherits(x, "sql")) return(x)
  }

  if (rlang::is_symbol(x)) x <- rlang::as_name(x)

  if (!is.character(x) || length(x) != 1) {
    cli::cli_abort(
      "{.arg x} must be a column name or a {.fn sql} expression, not {.obj_type_friendly {x}}."
    )
  }

  paste0('"', x, '"')
}

# Normalize a datetime-like column to a naive wall-clock TIMESTAMP in `tz`,
# regardless of its stored type. These three shapes look identical once
# collected through our connection but behave differently in SQL:
#   * TIMESTAMPTZ       -> instant; take the wall-clock time in `tz`
#   * TIMESTAMP (naive) -> assumed UTC (that is how POSIXct values land when a
#                          column is written without a tz), converted to `tz`
#   * DATE / other      -> tz-agnostic; cast straight to midnight TIMESTAMP
# Branching on typeof() keeps the result correct and session-tz independent.
# Returns a bare SQL fragment (character), not a `sql()` object.
sql_local_ts <- function(col_name, tz) {
  glue::glue(
    "CASE typeof({col_name}) ",
    "WHEN 'TIMESTAMP WITH TIME ZONE' THEN CAST({col_name} AT TIME ZONE '{tz}' AS TIMESTAMP) ",
    "WHEN 'TIMESTAMP' THEN CAST(({col_name} AT TIME ZONE 'UTC') AT TIME ZONE '{tz}' AS TIMESTAMP) ",
    "ELSE CAST({col_name} AS TIMESTAMP) END"
  )
}

#' Flattening function for use in dbplyr piplines
#'
#' Use with the `!!` operator to return the sql statement before translation
#'
#' @param x values to aggregate. Can also be a sql statement wrapped in sql()
#' @param sep Separator. Defaults to "; "
#' @param distinct Should values be made distinct?
#' @param order_by values to order by. Defaults to `x`
#'
#' @returns a `sql()` STRING_AGG statement
#' @export
#' @md
sql_flatten <- function(x, sep = "; ", distinct = TRUE, order_by = NULL) {
  env <- rlang::caller_env()
  col_name <- quote_col(rlang::enexpr(x), env)

  order_by <- rlang::enexpr(order_by)
  order_col <- if (!is.null(order_by)) quote_col(order_by, env) else col_name

  distinct_sql <- if (distinct) "DISTINCT " else ""

  dplyr::sql(glue::glue("STRING_AGG({distinct_sql}{col_name}, '{sep}' ORDER BY {order_col})"))
}


#' As Date with TZ function for dbplyr pipelines
#'
#' Use with the `!!` operator to return the sql statement before translation.
#' `sql_mo()` floors the date to the nearest month.
#'
#' Both functions accept `TIMESTAMPTZ`, naive `TIMESTAMP` (assumed UTC), and
#' `DATE` columns and return the calendar date/month in `tz`. The input type is
#' detected at runtime so the result is correct regardless of the session
#' timezone.
#'
#' @param x values to cast to date
#' @param tz timezone to cast at.
#'
#' @returns a `sql()` CAST statement
#' @export
#' @md
sql_as_date <- function(x, tz = "America/New_York") {
  col_name <- quote_col(rlang::enexpr(x), rlang::caller_env())
  local_ts <- sql_local_ts(col_name, tz)
  dplyr::sql(glue::glue("CAST({local_ts} AS DATE)"))
}

#' @export
#' @rdname sql_as_date
sql_mo <- function(x, tz = "America/New_York") {
  col_name <- quote_col(rlang::enexpr(x), rlang::caller_env())
  local_ts <- sql_local_ts(col_name, tz)
  dplyr::sql(glue::glue("CAST(DATE_TRUNC('month', {local_ts}) AS DATE)"))
}

#' Process DA-2 Date Strings
#'
#' These strings take the form of '0:2025112718300000:0.000000:126:0'
#'
#' @param x column with date string (ce_result_value)
#' @param tz Timezone (for date-times)
#'
#' @returns `sql_ce_dt_tm()` and `sql_ce_date()` return `sql()` statements;
#'   `ce_dt_tm()` and `ce_date()` return parsed date/datetime vectors.
#' @export
#' @md
sql_ce_dt_tm <- function(x, tz = "America/New_York") {
  col_name <- quote_col(rlang::enexpr(x), rlang::caller_env())
  dplyr::sql(glue::glue(
    "STRPTIME(SUBSTR({col_name}, 3, 14) || '{tz}', '%Y%m%d%H%M%S%Z')"
  ))
}

#' @rdname sql_ce_dt_tm
#' @export
ce_dt_tm <- function(x, tz = "America/New_York") {
  lubridate::ymd_hms(substr(x, 3, 16), tz = tz)
}

#' @rdname sql_ce_dt_tm
#' @export
ce_date <- function(x) {
  lubridate::ymd(substr(x, 3, 10))
}

#' @rdname sql_ce_dt_tm
#' @export
sql_ce_date <- function(x) {
  col_name <- quote_col(rlang::enexpr(x), rlang::caller_env())
  dplyr::sql(glue::glue(
    "STRPTIME(SUBSTR({col_name}, 3, 8), '%Y%m%d')::DATE"
  ))
}


#' Age at a given time
#'
#' `sql_age()` is designed to be used in lazy tibbles. Use with the `!!`
#' operator for local evaluation into a `sql()` string first.
#'
#' `dt` and `dob` may be `TIMESTAMPTZ`, naive `TIMESTAMP` (assumed UTC), or
#' `DATE`; both are normalized to a calendar date in `tz` before the age is
#' computed.
#'
#' @param dt date at which to find the age
#' @param dob date of birth
#' @param tz timezone used to resolve `dt`/`dob` to calendar dates.
#'
#' @returns `sql_age()` returns a sql() statement, `age()` returns an integer age
#' @export
#' @md
sql_age <- function(dt, dob, tz = "America/New_York") {

  env <- rlang::caller_env()
  dt <- quote_col(rlang::enexpr(dt), env)
  dob <- quote_col(rlang::enexpr(dob), env)

  dt_d <- glue::glue("CAST({sql_local_ts(dt, tz)} AS DATE)")
  dob_d <- glue::glue("CAST({sql_local_ts(dob, tz)} AS DATE)")

  dplyr::sql(glue::glue(
    "DATEDIFF('year', {dob_d}, {dt_d}) - ",
    "CASE WHEN STRFTIME({dt_d}, '%m-%d') < STRFTIME({dob_d}, '%m-%d') THEN 1 ELSE 0 END"
  ))
}

#' @export
#' @rdname sql_age
age <- function(dt, dob) {
  as.integer(lubridate::year(dt) - lubridate::year(dob) -
    (format(dt, "%m-%d") < format(dob, "%m-%d")))
}
