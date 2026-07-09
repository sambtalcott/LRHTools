# Helper: create a workbook with a named table starting at a given cell
make_test_wb <- function(df, table_name = "testtable", sheet = "Sheet1", dims = "A1") {
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet(sheet)
  wb$add_data_table(sheet = sheet, x = df, dims = dims, table_name = table_name)
  wb
}

# ── graph_http_status ────────────────────────────────────────────────────────

test_that("graph_http_status parses the HTTP code from an AzureGraph message", {
  e <- simpleError("Service Unavailable (HTTP 503). Failed to complete operation.")
  expect_equal(graph_http_status(e), 503L)
})

test_that("graph_http_status returns NA when no code is present", {
  expect_true(is.na(graph_http_status(simpleError("Timeout was reached [graph.microsoft.com]"))))
})

# ── graph_retry ──────────────────────────────────────────────────────────────

test_that("graph_retry returns the value and does not retry on success", {
  n <- 0
  res <- graph_retry(\() { n <<- n + 1; "ok" }, quiet = TRUE)
  expect_equal(res, "ok")
  expect_equal(n, 1)
})

test_that("graph_retry retries a transient 503 then succeeds", {
  n <- 0
  res <- graph_retry(\() {
    n <<- n + 1
    if (n < 3) stop(simpleError("Service Unavailable (HTTP 503)."))
    "done"
  }, base_wait = 0, max_wait = 0, quiet = TRUE)
  expect_equal(res, "done")
  expect_equal(n, 3)
})

test_that("graph_retry does not retry a non-transient 400", {
  n <- 0
  expect_error(
    graph_retry(\() { n <<- n + 1; stop(simpleError("Bad Request (HTTP 400).")) },
                base_wait = 0, max_wait = 0, quiet = TRUE),
    "HTTP 400"
  )
  expect_equal(n, 1)
})

test_that("graph_retry gives up after max_tries and re-throws", {
  n <- 0
  expect_error(
    graph_retry(\() { n <<- n + 1; stop(simpleError("Service Unavailable (HTTP 503).")) },
                max_tries = 3, base_wait = 0, max_wait = 0, quiet = TRUE),
    "HTTP 503"
  )
  expect_equal(n, 3)
})

test_that("graph_retry treats 504 as non-retryable for non-idempotent calls", {
  n <- 0
  expect_error(
    graph_retry(\() { n <<- n + 1; stop(simpleError("Gateway Timeout (HTTP 504).")) },
                idempotent = FALSE, base_wait = 0, max_wait = 0, quiet = TRUE),
    "HTTP 504"
  )
  expect_equal(n, 1)
})

test_that("graph_retry retries a non-idempotent 503 (request never landed)", {
  n <- 0
  res <- graph_retry(\() {
    n <<- n + 1
    if (n < 2) stop(simpleError("Service Unavailable (HTTP 503)."))
    "appended"
  }, idempotent = FALSE, base_wait = 0, max_wait = 0, quiet = TRUE)
  expect_equal(res, "appended")
  expect_equal(n, 2)
})

# ── graph_df_to_values ───────────────────────────────────────────────────────

test_that("graph_df_to_values converts a data frame to nested lists", {
  df <- data.frame(a = 1:2, b = c("x", "y"))
  result <- graph_df_to_values(df)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(result[[1]], list("1", "x"))
  expect_equal(result[[2]], list("2", "y"))
})

test_that("graph_df_to_values converts NAs to empty strings", {
  df <- data.frame(a = c(1, NA), b = c(NA, "y"))
  result <- graph_df_to_values(df)

  expect_equal(result[[1]], list("1", ""))
  expect_equal(result[[2]], list("", "y"))
})

test_that("graph_df_to_values formats dates and datetimes", {
  df <- data.frame(
    d = as.Date("2025-01-15"),
    dt = as.POSIXct("2025-01-15 10:30:00", tz = "UTC")
  )
  result <- graph_df_to_values(df)

  expect_equal(result[[1]][[1]], "2025-01-15")
  expect_equal(result[[1]][[2]], "2025-01-15 10:30:00")
})

# ── od_xl_compare ────────────────────────────────────────────────────────────

test_that("compare detects new rows as appends", {
  wb_df <- data.frame(id = 1:2, val = c("a", "b"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1:3, val = c("a", "b", "c"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$append), 1)
  expect_equal(result$append$id, 3)
  expect_equal(result$append$val, "c")
  expect_equal(nrow(result$patch), 0)
})

test_that("compare detects changed values as patches", {
  wb_df <- data.frame(id = 1:3, val = c("a", "b", "c"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1:3, val = c("a", "CHANGED", "c"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$append), 0)
  expect_equal(nrow(result$patch), 1)
  expect_equal(result$patch$col, "val")
  expect_equal(result$patch$old, "b")
  expect_equal(result$patch$new, "CHANGED")
})

test_that("compare computes correct cell ranges", {
  wb_df <- data.frame(id = 1:3, val = c("a", "b", "c"))
  wb <- make_test_wb(wb_df, dims = "A1")

  # Change row 2 (id=2), column "val" is column B
  x <- data.frame(id = 1:3, val = c("a", "CHANGED", "c"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  # Table starts at A1, header is row 1, data row 2 is Excel row 3, column B
  expect_equal(result$patch$range, "B3")
  expect_equal(result$patch$sheet, "Sheet1")
})

test_that("compare computes correct ranges with offset table", {
  wb_df <- data.frame(id = 1:3, x = c(10, 20, 30), y = c(40, 50, 60))
  wb <- make_test_wb(wb_df, dims = "C5", sheet = "Data")

  # Change row 3 (id=3) column y: col index 3 -> E, data row 3 -> Excel row 8
  x <- data.frame(id = 1:3, x = c(10, 20, 30), y = c(40, 50, 99))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(result$patch$range, "E8")
  expect_equal(result$patch$sheet, "Data")
})

test_that("compare handles both appends and patches together", {
  wb_df <- data.frame(id = 1:2, val = c("a", "b"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = c(1, 2, 3), val = c("CHANGED", "b", "new"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$append), 1)
  expect_equal(result$append$id, 3)
  expect_equal(nrow(result$patch), 1)
  expect_equal(result$patch$new, "CHANGED")
})

test_that("compare returns empty patch when no changes", {
  wb_df <- data.frame(id = 1:2, val = c("a", "b"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1:2, val = c("a", "b"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$append), 0)
  expect_equal(nrow(result$patch), 0)
})

test_that("compare treats \\r\\n and \\n as equal (no spurious patch)", {
  # Excel stores in-cell breaks as "\n"; CSV/DuckDB-sourced values often
  # carry Windows "\r\n". Without line-ending normalization the byte-exact
  # compare flags every multi-line cell every run, never converges, and the
  # resulting patch can be large enough to time out (HTTP 504).
  wb_df <- data.frame(id = 1:2, note = c("leave.\r\nNicole", "ok\rbye"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1:2, note = c("leave.\nNicole", "ok\nbye"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$append), 0)
  expect_equal(nrow(result$patch), 0)
})

test_that("compare still detects a real change in a multi-line cell", {
  # Normalization must not mask genuine edits beyond the line ending.
  wb_df <- data.frame(id = 1:2, note = c("leave.\r\nNicole", "keep"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1:2, note = c("leave.\nNicole EDITED", "keep"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$patch), 1)
  expect_equal(result$patch$col, "note")
  expect_equal(result$patch$new, "leave.\nNicole EDITED")
})

test_that("compare fills missing columns with NA for appends", {
  wb_df <- data.frame(id = 1, val = "a", extra = "z")
  wb <- make_test_wb(wb_df)

  # x doesn't have "extra" column
  x <- data.frame(id = 2, val = "b")

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$append), 1)
  expect_equal(colnames(result$append), c("id", "val", "extra"))
  expect_true(is.na(result$append$extra))
})

test_that("compare detects missing rows as removes with 0-based indices", {
  wb_df <- data.frame(id = 1:3, val = c("a", "b", "c"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = c(1L, 3L), val = c("a", "c"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$remove), 1)
  expect_equal(result$remove$id, 2)
  expect_equal(result$remove$index, 1L)
})

test_that("compare ignores the blank placeholder row left by removing all rows", {
  # After od_xl_remove() deletes every data row, Excel keeps a single blank
  # row inside the table ref. It reads back as all-NA but is not a real
  # member of the Graph table-rows collection, so flagging it for removal
  # makes the next od_xl_remove() error. It must not appear in $remove; the
  # next append writes over it.
  wb_df <- data.frame(id = NA_integer_, val = NA_character_)
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1:2, val = c("a", "b"))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$remove), 0)
  expect_equal(nrow(result$append), 2)
})

test_that("compare skips only all-NA rows; real removes keep their indices", {
  wb_df <- data.frame(id = c(1L, NA, 3L), val = c("a", NA, "c"))
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1L, val = "a")

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list())

  expect_equal(nrow(result$remove), 1)
  expect_equal(result$remove$id, 3)
  expect_equal(result$remove$index, 2L)
})

test_that("compare errors on missing columns in x", {
  wb_df <- data.frame(id = 1, val = "a")
  wb <- make_test_wb(wb_df)

  x <- data.frame(id = 1, bad_col = "a")

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  expect_error(
    od_xl_compare(x, "test.xlsx", "testtable", id_cols = "id", od = list()),
    "Column names"
  )
})

test_that("compare errors on non-xlsx path", {
  local_mocked_bindings(od_exists = function(...) TRUE)

  expect_error(
    od_xl_compare(data.frame(), "test.csv", "T", id_cols = "id", od = list()),
    "can only be used on .xlsx"
  )
})

test_that("compare errors on missing table", {
  wb_df <- data.frame(id = 1, val = "a")
  wb <- make_test_wb(wb_df, table_name = "realtable")

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  expect_error(
    od_xl_compare(data.frame(id = 1), "test.xlsx", "faketable", id_cols = "id", od = list()),
    "No table with the name"
  )
})

# ── od_xl_patch ──────────────────────────────────────────────────────────────

test_that("patch errors on missing columns", {
  local_mocked_bindings(
    od_exists = function(...) TRUE
  )

  expect_error(
    od_xl_patch(data.frame(sheet = "S1", range = "A1"), "test.xlsx", od = list()),
    "missing required column"
  )
})

test_that("patch returns NULL on empty input", {
  local_mocked_bindings(
    od_exists = function(...) TRUE
  )

  result <- od_xl_patch(
    data.frame(sheet = character(0), range = character(0), new = character(0)),
    "test.xlsx", od = list()
  )

  expect_null(result)
})

test_that("patch coerces non-character new column", {
  calls <- list()
  mock_item <- list(
    do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) }
  )
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(
    od_exists = function(...) TRUE
  )

  x <- data.frame(sheet = "Sheet1", range = "A2", new = 42)
  od_xl_patch(x, "test.xlsx", od = mock_od)

  # Verify the value was passed as character
  expect_equal(calls[[1]]$body$values, list(list("42")))
})

test_that("patch coerces Date values", {
  calls <- list()
  mock_item <- list(
    do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) }
  )
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(
    od_exists = function(...) TRUE
  )

  x <- data.frame(sheet = "Sheet1", range = "A2", new = as.Date("2025-06-15"))
  od_xl_patch(x, "test.xlsx", od = mock_od)

  expect_equal(calls[[1]]$body$values, list(list("2025-06-15")))
})

test_that("patch errors on non-xlsx path", {
  local_mocked_bindings(od_exists = function(...) TRUE)

  expect_error(
    od_xl_patch(data.frame(sheet = "S", range = "A1", new = "x"), "f.csv", od = list()),
    "can only be used on .xlsx"
  )
})

# ── od_xl_append ─────────────────────────────────────────────────────────────

test_that("append errors on column name mismatch", {
  wb_df <- data.frame(id = 1, val = "a")
  wb <- make_test_wb(wb_df)

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )
  mock_od <- list(get_item = function(...) list())

  expect_error(
    od_xl_append(data.frame(id = 1, wrong = "a"), "test.xlsx", "testtable", od = mock_od),
    "Column names"
  )
})

test_that("append errors on column count mismatch", {
  wb_df <- data.frame(id = 1, val = "a")
  wb <- make_test_wb(wb_df)

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )
  mock_od <- list(get_item = function(...) list())

  expect_error(
    od_xl_append(data.frame(id = 1, val = "a", extra = "b"), "test.xlsx", "testtable",
                 od = mock_od, check_columns = FALSE),
    "column"
  )
})

test_that("append returns early on empty input", {
  wb_df <- data.frame(id = 1, val = "a")
  wb <- make_test_wb(wb_df)

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )
  mock_item <- list()
  mock_od <- list(get_item = function(...) mock_item)

  result <- od_xl_append(data.frame(id = integer(0), val = character(0)),
                         "test.xlsx", "testtable", od = mock_od)

  expect_identical(result, mock_item)
})
