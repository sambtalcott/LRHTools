test_that("appends data", {
  t1 <- tibble::tibble(a = 1, b = 2)
  t2 <- tibble::tibble(a = 3, b = 4)
  t3 <- dplyr::bind_rows(t1, t2)

  rds_file <- tempfile(fileext = ".rds")
  readr::write_rds(t1, rds_file)
  x <- append_rds(t2, rds_file)
  expect_equal(x, t3)
  expect_equal(readr::read_rds(rds_file), t3)
})

test_that("handles column type mismatches gracefully", {
  t1 <- tibble::tibble(a = 1:2, b = c("a", "b"))
  t2 <- tibble::tibble(a = 3:4, b = 5:6)

  rds_file <- tempfile(fileext = ".rds")
  readr::write_rds(t1, rds_file)
  on.exit(file.remove(rds_file), add = TRUE)

  expect_error(append_rds(t2, rds_file), "Can't combine")
})

test_that("handles new columns gracefully", {
  t1 <- tibble::tibble(a = 1:2, b = c("a", "b"))
  t2 <- tibble::tibble(a = 3:4, b = c("c", "d"), c = TRUE)

  rds_file <- tempfile(fileext = ".rds")
  readr::write_rds(t1, rds_file)
  on.exit(file.remove(rds_file), add = TRUE)

  # Error if file is incomplete
  expect_error(append_rds(t2, rds_file), "contains new column")
  # Can be over-ridden with add_cols
  expect_invisible(append_rds(t2, rds_file, add_cols = TRUE))

  # Warning if x is incomplete
  readr::write_rds(t2, rds_file)
  expect_warning(append_rds(t1, rds_file), "Current file includes column")
})
