# Tests for the date helpers sql_as_date(), sql_mo() and sql_age().
#
# The tricky part these guard: a column may reach DuckDB as a TIMESTAMPTZ, a
# naive TIMESTAMP (which our connection writes as UTC wall-clock and which then
# previews/collects identically to a TIMESTAMPTZ), or a DATE. All three must
# resolve to the same calendar date/month in the target timezone, and the
# result must not depend on the session TimeZone.

# Build a one-row probe table with each column stored as a distinct type.
# Instants are chosen to straddle NY midnight / a month boundary / a birthday.
make_probe <- function(con) {
  DBI::dbExecute(con, "DROP TABLE IF EXISTS DT_PROBE")
  DBI::dbExecute(con, "
    CREATE TABLE DT_PROBE AS SELECT
      -- 2025-07-15 03:00 UTC == 2025-07-14 23:00 America/New_York
      TIMESTAMPTZ '2025-07-15 03:00:00+00' AS tstz,
      TIMESTAMP   '2025-07-15 03:00:00'    AS ts,     -- naive, assumed UTC
      DATE        '2025-07-15'             AS d,
      -- 2025-02-01 02:00 UTC == 2025-01-31 21:00 America/New_York
      TIMESTAMPTZ '2025-02-01 02:00:00+00' AS mo_tstz,
      TIMESTAMP   '2025-02-01 02:00:00'    AS mo_ts,
      DATE        '2025-02-01'             AS mo_d,
      -- age fixtures
      DATE        '2000-06-15'             AS dob,
      DATE        '2000-02-29'             AS dob_leap,
      TIMESTAMPTZ '2025-06-15 03:00:00+00' AS dt_tstz,  -- NY 2025-06-14 (bday-1)
      TIMESTAMP   '2025-06-15 03:00:00'    AS dt_ts,     -- naive UTC -> NY 06-14
      DATE        '2025-06-15'             AS dt_date,   -- exact birthday
      DATE        '2025-02-28'             AS feb28,
      DATE        '2025-03-01'             AS mar01
  ")
}

test_that("sql_as_date resolves every input type to the tz calendar date", {
  con <- lrh_con(db_file = ":memory:", type = "read_write")
  on.exit(lrh_disconnect(), add = TRUE)
  make_probe(con)

  res <- dplyr::tbl(con, "DT_PROBE") |>
    dplyr::mutate(
      from_tstz = !!sql_as_date(tstz),
      from_ts   = !!sql_as_date(ts),
      from_date = !!sql_as_date(d)
    ) |>
    dplyr::collect()

  # TIMESTAMPTZ and naive-UTC both fall back a day into NY; a DATE is unchanged.
  expect_equal(res$from_tstz, as.Date("2025-07-14"))
  expect_equal(res$from_ts,   as.Date("2025-07-14"))
  expect_equal(res$from_date, as.Date("2025-07-15"))
})

test_that("sql_as_date honors a non-default tz argument", {
  con <- lrh_con(db_file = ":memory:", type = "read_write")
  on.exit(lrh_disconnect(), add = TRUE)
  make_probe(con)

  res <- dplyr::tbl(con, "DT_PROBE") |>
    dplyr::mutate(
      la = !!sql_as_date(tstz, tz = "America/Los_Angeles")
    ) |>
    dplyr::collect()

  # 2025-07-15 03:00 UTC == 2025-07-14 20:00 in LA
  expect_equal(res$la, as.Date("2025-07-14"))
})

test_that("sql_mo floors to the first of the tz month for every input type", {
  con <- lrh_con(db_file = ":memory:", type = "read_write")
  on.exit(lrh_disconnect(), add = TRUE)
  make_probe(con)

  res <- dplyr::tbl(con, "DT_PROBE") |>
    dplyr::mutate(
      from_tstz = !!sql_mo(mo_tstz),
      from_ts   = !!sql_mo(mo_ts),
      from_date = !!sql_mo(mo_d)
    ) |>
    dplyr::collect()

  # The instant is Jan 31 in NY, so tstz/naive-UTC floor to January; the bare
  # DATE is a February date and floors to February.
  expect_equal(res$from_tstz, as.Date("2025-01-01"))
  expect_equal(res$from_ts,   as.Date("2025-01-01"))
  expect_equal(res$from_date, as.Date("2025-02-01"))
})

test_that("sql_age handles birthdays, boundaries and leap-day births", {
  con <- lrh_con(db_file = ":memory:", type = "read_write")
  on.exit(lrh_disconnect(), add = TRUE)
  make_probe(con)

  res <- dplyr::tbl(con, "DT_PROBE") |>
    dplyr::mutate(
      age_date = !!sql_age(dt_date, dob),   # exact birthday -> 25
      age_tstz = !!sql_age(dt_tstz, dob),   # NY 06-14        -> 24
      age_ts   = !!sql_age(dt_ts, dob),     # naive UTC 06-14 -> 24
      age_feb28 = !!sql_age(feb28, dob_leap), # leap dob, not yet -> 24
      age_mar01 = !!sql_age(mar01, dob_leap)  # leap dob, turns   -> 25
    ) |>
    dplyr::collect()

  expect_equal(res$age_date, 25)
  expect_equal(res$age_tstz, 24)
  expect_equal(res$age_ts,   24)
  expect_equal(res$age_feb28, 24)
  expect_equal(res$age_mar01, 25)
})

test_that("results do not depend on the session TimeZone", {
  con <- lrh_con(db_file = ":memory:", type = "read_write")
  on.exit(lrh_disconnect(), add = TRUE)
  make_probe(con)
  DBI::dbExecute(con, "SET TimeZone = 'UTC'")

  res <- dplyr::tbl(con, "DT_PROBE") |>
    dplyr::mutate(
      ad_tstz = !!sql_as_date(tstz),
      ad_ts   = !!sql_as_date(ts),
      ad_date = !!sql_as_date(d),
      age_ts  = !!sql_age(dt_ts, dob)
    ) |>
    dplyr::collect()

  expect_equal(res$ad_tstz, as.Date("2025-07-14"))
  expect_equal(res$ad_ts,   as.Date("2025-07-14"))
  expect_equal(res$ad_date, as.Date("2025-07-15"))
  expect_equal(res$age_ts,  24)
})
