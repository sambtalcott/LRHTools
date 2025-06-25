test_that("writes if a table doesn't exist", {
  t1 <- data.frame(a = 1, b = 2)

  con <- lrh_con(db_file = ":memory:", type = "read_write")
  append_duckdb(t1, "TEST")
  expect_equal(DBI::dbReadTable(con, "TEST"), t1)
  lrh_disconnect()
})

test_that("appends if a table does exist", {
  t1 <- data.frame(a = 1, b = 2)
  t2 <- data.frame(a = 3, b = 4)
  t3 <- dplyr::bind_rows(t1, t2) |> dplyr::arrange(a, b)

  con <- lrh_con(db_file = ":memory:", type = "read_write")

  append_duckdb(t1, "TEST")
  append_duckdb(t2, "TEST")

  # NOTE: Row order is NOT preserved. Do not rely on it.
  t4 <- DBI::dbReadTable(con, "TEST") |> dplyr::arrange(a, b)
  expect_equal(t3, t4)
  lrh_disconnect()
})

test_that("handles column type mismatches gracefully", {
  t1 <- tibble::tibble(a = 3:4, b = 5:6)
  t2 <- tibble::tibble(a = 1:2, b = c("a", "b"))

  con <- lrh_con(db_file = ":memory:", type = "read_write")

  DBI::dbWriteTable(con, "TEST", t1)
  expect_error(append_duckdb(t2, "TEST"), "Could not convert")

  # NOTE: It will coerce types if possible (int -> char), but won't coerce if
  # not possible (char -> int)
  lrh_disconnect()
})

test_that("handles new columns gracefully", {
  t1 <- tibble::tibble(a = 1:2, b = c("a", "b"))
  t2 <- tibble::tibble(a = 3:4, b = c("c", "d"), c = TRUE)

  con <- lrh_con(db_file = ":memory:", type = "read_write")

  # Error if file is incomplete
  DBI::dbWriteTable(con, "TEST", t1)
  expect_error(append_duckdb(t2, "TEST"), "does not exist")

  # No error if x is incomplete
  DBI::dbWriteTable(con, "TEST2", t2)
  expect_equal(append_duckdb(t2, "TEST2"), 0)
  lrh_disconnect()
})

test_that("lastupdate works as expected", {

  # Temporary database
  con <- lrh_con(db_file = ":memory:", type = "read_write")

  # Example data frames
  d1 <- data.frame(
    x = 1:5,
    x_update = 10:6
  )
  d2 <- cbind(d1, x_update_dt_tm = c(4,5,8,2,3))
  d3 <- cbind(d2, y_update_dt_tm = 2:6)

  DBI::dbWriteTable(con, "D1", d1)
  DBI::dbWriteTable(con, "D2", d2)
  DBI::dbWriteTable(con, "D3", d3)

  # No matching column
  expect_error(lastupdate_duckdb("D1"), "no columns containing")
  expect_equal(lastupdate_duckdb("D1", dt_col = "x_update"), 10)

  # Exactly one matching column
  expect_equal(lastupdate_duckdb("D2"), 8)
  expect_equal(lastupdate_duckdb("D2", dt_col = "x"), 5)

  # More than one matching column
  expect_error(lastupdate_duckdb("D3"), "multiple possible columns")
  expect_equal(lastupdate_duckdb("D3", dt_col = "y_update_dt_tm"), 6)

  # Test alert
  expect_message(lastupdate_alert_duckdb("D2"), "last updated on")

  # Test atleast
  lastupdate_min_duckdb("D2", 7) # Expect no error
  expect_error(lastupdate_min_duckdb("D2", 9), "last updated on")
  lrh_disconnect()
})
