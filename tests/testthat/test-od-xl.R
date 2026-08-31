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

# ── resolve_xl_table ─────────────────────────────────────────────────────────

# Helper: workbook with one table per (sheet, name, dims) spec
make_multi_wb <- function(specs) {
  wb <- openxlsx2::wb_workbook()
  for (sh in unique(vapply(specs, `[[`, "", "sheet"))) wb$add_worksheet(sh)
  for (sp in specs) {
    wb$add_data_table(sheet = sp$sheet, x = data.frame(id = 1:2),
                      table_name = sp$name, dims = sp$dims %||% "A1")
  }
  wb
}

test_that("resolve_xl_table returns an explicit table", {
  wb <- make_test_wb(data.frame(id = 1), table_name = "realtable")
  expect_equal(resolve_xl_table(wb, table = "realtable"), "realtable")
})

test_that("resolve_xl_table errors on a table that isn't in the file", {
  wb <- make_test_wb(data.frame(id = 1), table_name = "realtable")
  expect_error(resolve_xl_table(wb, table = "faketable"), "No table with the name")
})

test_that("resolve_xl_table finds the single table on the first sheet", {
  wb <- make_test_wb(data.frame(id = 1), table_name = "onlytable", sheet = "Log")
  expect_equal(resolve_xl_table(wb), "onlytable")
})

test_that("resolve_xl_table finds the single table on a named sheet", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "Two", name = "second_tbl")
  ))
  expect_equal(resolve_xl_table(wb, sheet = "Two"), "second_tbl")
})

test_that("resolve_xl_table accepts a sheet index", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "Two", name = "second_tbl")
  ))
  expect_equal(resolve_xl_table(wb, sheet = 2), "second_tbl")
})

test_that("resolve_xl_table errors when a sheet has more than one table", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "One", name = "second_tbl", dims = "D1")
  ))
  expect_error(resolve_xl_table(wb, sheet = "One"), "has 2 tables")
})

test_that("resolve_xl_table errors when a sheet has no table", {
  wb <- make_test_wb(data.frame(id = 1), table_name = "onlytable", sheet = "One")
  wb$add_worksheet("Empty")
  expect_error(resolve_xl_table(wb, sheet = "Empty"), "No Excel Table found on sheet")
})

test_that("resolve_xl_table errors on an unknown sheet name or out-of-range index", {
  wb <- make_test_wb(data.frame(id = 1), table_name = "onlytable", sheet = "One")
  expect_error(resolve_xl_table(wb, sheet = "Nope"), "No sheet named")
  expect_error(resolve_xl_table(wb, sheet = 3), "out of range")
})

test_that("resolve_xl_table errors when the file has no tables", {
  wb <- openxlsx2::wb_workbook()$add_worksheet("One")
  expect_error(resolve_xl_table(wb), "No Excel Tables found")
})

test_that("resolve_xl_table errors on a duplicated table name", {
  # openxlsx2's clone_worksheet() gives every clone past the second the same
  # table name; Graph then rejects the whole file with HTTP 501.
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "dup_tbl"),
    list(sheet = "Two", name = "other_tbl")
  ))
  wb$tables$tab_name[wb$tables$tab_name == "other_tbl"] <- "dup_tbl"

  expect_error(resolve_xl_table(wb, sheet = "Two"), "appears 2 times")
  expect_error(resolve_xl_table(wb, table = "dup_tbl"), "appears 2 times")
})

test_that("resolve_xl_table keeps the legacy Table1 default over the first sheet", {
  # Table1 lives on the second sheet, and the first sheet is ambiguous:
  # without the legacy default this would error.
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "One", name = "second_tbl", dims = "D1"),
    list(sheet = "Two", name = "Table1")
  ))
  expect_equal(tolower(resolve_xl_table(wb)), "table1")
})

test_that("resolve_xl_table ignores deleted tables", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "live_tbl"),
    list(sheet = "One", name = "dead_tbl", dims = "D1")
  ))
  wb$tables$tab_act[wb$tables$tab_name == "dead_tbl"] <- 0
  expect_equal(resolve_xl_table(wb, sheet = "One"), "live_tbl")
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
    "can only be used on"
  )
})

test_that("check_xl_ext accepts .xlsx and .xlsm but not .xls or other types", {
  expect_null(check_xl_ext("a.xlsx", "f()"))
  expect_null(check_xl_ext("a.xlsm", "f()"))

  # .xls is the old binary format; Graph's workbook API can't touch it
  expect_error(check_xl_ext("a.xls", "f()"), "can only be used on")
  expect_error(check_xl_ext("a.csv", "f()"), "can only be used on")

  # the calling function's name should make it into the message
  expect_error(check_xl_ext("a.csv", "od_xl_sort()"), "od_xl_sort")
})

test_that("compare works on an .xlsm path", {
  wb_df <- data.frame(id = 1, val = "a")
  wb <- make_test_wb(wb_df)

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  res <- od_xl_compare(data.frame(id = 2, val = "b"), "macros.xlsm",
                       "testtable", id_cols = "id", od = list())

  expect_equal(nrow(res$append), 1)
  expect_equal(res$append$id, 2)
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

test_that("compare resolves the table from a sheet name", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "Two", name = "second_tbl")
  ))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(data.frame(id = 1:3), "test.xlsx", sheet = "Two",
                          id_cols = "id", od = list())

  expect_equal(result$table, "second_tbl")
  expect_equal(result$append$id, 3)
})

test_that("compare errors when the sheet holds more than one table", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "One", name = "second_tbl", dims = "D1")
  ))

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  expect_error(
    od_xl_compare(data.frame(id = 1), "test.xlsx", sheet = "One",
                  id_cols = "id", od = list()),
    "has 2 tables"
  )
})

test_that("compare defaults to the only table when none is named", {
  wb <- make_test_wb(data.frame(id = 1:2), table_name = "onlytable", sheet = "Log")

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  result <- od_xl_compare(data.frame(id = 1:2), "test.xlsx", id_cols = "id", od = list())

  expect_equal(result$table, "onlytable")
  expect_equal(nrow(result$append), 0)
})

# ── od_xl_sort ───────────────────────────────────────────────────────────────

test_that("sort resolves the table from a sheet name", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "Two", name = "second_tbl")
  ))
  calls <- list()
  mock_item <- list(do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) })
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  od_xl_sort("test.xlsx", columns = "id", sheet = "Two", od = mock_od)

  expect_length(calls, 1)
  expect_match(calls[[1]][[1]], "tables('second_tbl')/sort/apply", fixed = TRUE)
  expect_equal(calls[[1]]$body$fields, list(list(key = 0L, ascending = TRUE)))
})

test_that("sort defaults to the only table when none is named", {
  wb <- make_test_wb(data.frame(id = 1:2), table_name = "onlytable", sheet = "Log")
  calls <- list()
  mock_item <- list(do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) })
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  od_xl_sort("test.xlsx", columns = "id", od = mock_od)

  expect_match(calls[[1]][[1]], "tables('onlytable')/sort/apply", fixed = TRUE)
})

test_that("sort still takes table, columns and desc positionally", {
  wb <- make_test_wb(data.frame(id = 1:2, val = c("a", "b")), table_name = "sorttable")
  calls <- list()
  mock_item <- list(do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) })
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )

  od_xl_sort("test.xlsx", "sorttable", "val", TRUE, od = mock_od)

  expect_match(calls[[1]][[1]], "tables('sorttable')/sort/apply", fixed = TRUE)
  expect_equal(calls[[1]]$body$fields, list(list(key = 1L, ascending = FALSE)))
})

test_that("sort errors when the sheet holds more than one table", {
  wb <- make_multi_wb(list(
    list(sheet = "One", name = "first_tbl"),
    list(sheet = "One", name = "second_tbl", dims = "D1")
  ))
  local_mocked_bindings(
    od_exists = function(...) TRUE,
    od_read = function(...) wb
  )
  expect_error(
    od_xl_sort("test.xlsx", columns = "id", sheet = "One", od = list(get_item = function(...) list())),
    "has 2 tables"
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

test_that("blocks merge a contiguous column into one range", {
  x <- data.frame(sheet = "S1", range = paste0("B", 2:5), new = letters[1:4])
  b <- xl_patch_blocks(x)

  expect_equal(nrow(b), 1)
  expect_equal(b$address, "B2:B5")
  expect_equal(b$values[[1]], list(list("a"), list("b"), list("c"), list("d")))
})

test_that("blocks merge a contiguous rectangle into one range", {
  x <- data.frame(
    sheet = "S1",
    range = c("B2", "C2", "B3", "C3"),
    new = c("a", "b", "c", "d")
  )
  b <- xl_patch_blocks(x)

  expect_equal(nrow(b), 1)
  expect_equal(b$address, "B2:C3")
  expect_equal(b$values[[1]], list(list("a", "b"), list("c", "d")))
})

test_that("blocks never bridge a gap", {
  # B4 missing: a bounding box would blank it out
  x <- data.frame(sheet = "S1", range = c("B2", "B3", "B5"), new = c("a", "b", "c"))
  b <- xl_patch_blocks(x)

  expect_equal(b$address, c("B2:B3", "B5"))
  expect_equal(b$values, list(list(list("a"), list("b")), list(list("c"))))
})

test_that("blocks don't merge strips with different column spans", {
  x <- data.frame(
    sheet = "S1",
    range = c("B2", "C2", "B3"),
    new = c("a", "b", "c")
  )
  b <- xl_patch_blocks(x)

  expect_equal(sort(b$address), c("B2:C2", "B3"))
})

test_that("blocks are keyed by address, not input order", {
  x <- data.frame(
    sheet = "S1",
    range = c("C3", "B2", "C2", "B3"),
    new = c("d", "a", "b", "c")
  )
  b <- xl_patch_blocks(x)

  expect_equal(b$address, "B2:C3")
  expect_equal(b$values[[1]], list(list("a", "b"), list("c", "d")))
})

test_that("blocks are split at the row cap", {
  x <- data.frame(sheet = "S1", range = paste0("A", 1:5), new = as.character(1:5))
  b <- xl_patch_blocks(x, max_rows = 2L)

  expect_equal(b$address, c("A1:A2", "A3:A4", "A5"))
})

test_that("blocks keep sheets separate", {
  x <- data.frame(sheet = c("S1", "S2"), range = c("A1", "A2"), new = c("a", "b"))
  b <- xl_patch_blocks(x)

  expect_equal(nrow(b), 2)
  expect_equal(b$sheet, c("S1", "S2"))
})

test_that("blocks error on multi-cell or duplicated addresses", {
  expect_error(
    xl_patch_blocks(data.frame(sheet = "S1", range = "A1:B2", new = "a")),
    "single-cell address"
  )
  expect_error(
    xl_patch_blocks(data.frame(sheet = "S1", range = c("A1", "A1"), new = c("a", "b"))),
    "more than one value for the same cell"
  )
})

test_that("patch batches contiguous cells into one request", {
  calls <- list()
  mock_item <- list(
    do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) }
  )
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(od_exists = function(...) TRUE)

  x <- data.frame(sheet = "Sheet1", range = paste0("B", 2:4), new = c("a", "b", "c"))
  od_xl_patch(x, "test.xlsx", od = mock_od)

  expect_length(calls, 1)
  expect_match(calls[[1]][[1]], "address='B2:B4'", fixed = TRUE)
  expect_equal(calls[[1]]$body$values, list(list("a"), list("b"), list("c")))
})

test_that("patch with use_blocks = FALSE writes one cell per request", {
  calls <- list()
  mock_item <- list(
    do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) }
  )
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(od_exists = function(...) TRUE)

  x <- data.frame(sheet = "Sheet1", range = paste0("B", 2:4), new = c("a", "b", "c"))
  od_xl_patch(x, "test.xlsx", od = mock_od, use_blocks = FALSE)

  expect_length(calls, 3)
  expect_equal(calls[[1]]$body$values, list(list("a")))
})

test_that("patch clears NA values as empty strings within a block", {
  calls <- list()
  mock_item <- list(
    do_operation = function(...) { calls[[length(calls) + 1]] <<- list(...) }
  )
  mock_od <- list(get_item = function(...) mock_item)

  local_mocked_bindings(od_exists = function(...) TRUE)

  x <- data.frame(sheet = "Sheet1", range = c("B2", "B3"), new = c("a", NA))
  od_xl_patch(x, "test.xlsx", od = mock_od)

  expect_equal(calls[[1]]$body$values, list(list("a"), list("")))
})

test_that("patch errors on non-xlsx path", {
  local_mocked_bindings(od_exists = function(...) TRUE)

  expect_error(
    od_xl_patch(data.frame(sheet = "S", range = "A1", new = "x"), "f.csv", od = list()),
    "can only be used on"
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
