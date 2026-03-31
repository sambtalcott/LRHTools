# Helper: create a workbook with a named table starting at a given cell
make_test_wb <- function(df, table_name = "testtable", sheet = "Sheet1", dims = "A1") {
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet(sheet)
  wb$add_data_table(sheet = sheet, x = df, dims = dims, table_name = table_name)
  wb
}

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
