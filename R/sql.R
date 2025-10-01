

#' Flattening function for use in dbplyr piplines
#'
#' Use with the `!!` operator to return the sql statement before translation
#'
#' @param x values to aggregate
#' @param sep Separator. Defaults to "; "
#' @param distinct Should values be made distinct?
#' @param order_by values to order by. Defaults to `x`
#'
#' @returns a `sql()` STRING_AGG statement
#' @export
#' @md
sql_flatten <- function(x, sep = "; ", distinct = TRUE, order_by = NULL) {
  col_name <- as.character(substitute(x))
  order_col <- order_by %||% col_name  # order by the column itself if not specified
  order_col <- as.character(substitute(order_col))

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
  dplyr::sql(glue::glue(
      "CAST({x} AT TIME ZONE '{tz}' AS DATE)"
  ))
}
