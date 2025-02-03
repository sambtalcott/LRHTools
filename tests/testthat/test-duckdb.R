test_that("writes if a table doesn't exist", {
  t1 <- data.frame(a = 1, b = 2)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  append_duckdb(t1, "TEST", con = con)
  expect_equal(DBI::dbReadTable(con, "TEST"), t1)
})

test_that("appends if a table does exist", {
  t1 <- data.frame(a = 1, b = 2)
  t2 <- data.frame(a = 3, b = 4)
  t3 <- dplyr::bind_rows(t1, t2) |> dplyr::arrange(a, b)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  append_duckdb(t1, "TEST", con = con)
  append_duckdb(t2, "TEST", con = con)

  # NOTE: Row order is NOT preserved. Do not rely on it.
  t4 <- DBI::dbReadTable(con, "TEST") |> dplyr::arrange(a, b)
  expect_equal(t3, t4)
})

test_that("handles column type mismatches gracefully", {
  t1 <- tibble::tibble(a = 3:4, b = 5:6)
  t2 <- tibble::tibble(a = 1:2, b = c("a", "b"))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "TEST", t1)
  expect_error(append_duckdb(t2, "TEST", con = con), "Could not convert")

  # NOTE: It will coerce types if possible (int -> char), but won't coerce if
  # not possible (char -> int)
})

test_that("handles new columns gracefully", {
  t1 <- tibble::tibble(a = 1:2, b = c("a", "b"))
  t2 <- tibble::tibble(a = 3:4, b = c("c", "d"), c = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Error if file is incomplete
  DBI::dbWriteTable(con, "TEST", t1)
  expect_error(append_duckdb(t2, "TEST", con = con), "does not exist")

  # No error if x is incomplete
  DBI::dbWriteTable(con, "TEST2", t2)
  expect_equal(append_duckdb(t2, "TEST2", con = con), 0)
})
