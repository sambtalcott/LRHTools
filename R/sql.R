
# Helper to quote column names for SQL (handles spaces and special characters)
quote_col <- function(x) {
  if (inherits(x, "sql")) return(x)
  x <- as.character(x)
  paste0('"', x, '"')
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
  x <- rlang::enexpr(x)
  col_name <- if (rlang::is_call(x)) quote_col(eval(x)) else quote_col(x)

  order_by <- rlang::enexpr(order_by)
  order_col <- if (!is.null(order_by)) quote_col(order_by) else col_name

  distinct_sql <- if (distinct) "DISTINCT " else ""

  dplyr::sql(glue::glue("STRING_AGG({distinct_sql}{col_name}, '{sep}' ORDER BY {order_col})"))
}


#' As Date with TZ function for dbplyr pipelines
#'
#' Use with the `!!` operator to return the sql statement before translation
#'
#' @param x values to cast to date
#' @param tz timezone to cast at.
#'
#' @returns a `sql()` CAST statement
#' @export
#' @md
sql_as_date <- function(x, tz = "America/New_York") {
  col_name <- quote_col(rlang::enexpr(x))
  dplyr::sql(glue::glue(
      "CAST({col_name} AT TIME ZONE '{tz}' AS DATE)"
  ))
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
  col_name <- quote_col(rlang::enexpr(x))
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
  col_name <- quote_col(rlang::enexpr(x))
  dplyr::sql(glue::glue(
    "STRPTIME(SUBSTR({col_name}, 3, 8), '%Y%m%d')::DATE"
  ))
}


#' Age at a given time
#'
#' `sql_age()` is designed to be used in lazy tibbles. Use with the `!!`
#' operator for local evaluation into a `sql()` string first.
#'
#' @param dt date at which to find the age
#' @param dob date of birth
#'
#' @returns `sql_age()` returns a sql() statement, `age()` returns an integer age
#' @export
#' @md
sql_age <- function(dt, dob) {

  dt <- quote_col(rlang::enexpr(dt))
  dob <- quote_col(rlang::enexpr(dob))

  dplyr::sql(glue::glue(
    "DATEDIFF('year', {dob}, {dt}) -
    CASE WHEN STRFTIME({dt}, '%m-%d') < STRFTIME({dob}, '%m-%d') THEN 1 ELSE 0 END"
  ))
}

#' @export
#' @rdname sql_age
age <- function(dt, dob) {
  as.integer(lubridate::year(dt) - lubridate::year(dob) -
    (format(dt, "%m-%d") < format(dob, "%m-%d")))
}
