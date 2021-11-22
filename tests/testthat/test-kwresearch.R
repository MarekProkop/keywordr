test_that("kwresearch() creates an empty kwresearch object", {
  kwr <- kwresearch()
  expect_equal(kwr$status, "empty")
  expect_s3_class(kwr, "kwresearch")
})

test_that("kwresearch(df) cretaes a kwresearch object with an initial data set", {
  df_minimal <- data.frame(
    query = c("seo", "seo", "keyword research"),
    volume = c(1000, 900, 500)
  )
  df_minimal_expected <- tibble::tibble(
    query = c("keyword research", "seo"),
    input =  NA_character_,
    source = NA_character_,
    volume = c(500, 900),
    cpc = NA_real_
  )
  kwr <- kwresearch(df_minimal)
  expect_equal(kwr$sourceData, df_minimal_expected)
  expect_equal(kwr$status, "data")
})

test_that("kwr_import_mm(.) |> kwresearch() creates kwresearch from MM files", {
  expected <- tibble::tibble(
    query = c("query 1", "query 1", "query 2", "query 2", "query 3"),
    input = c("input 1", "input 2", "input 1", "input 1", "input 2"),
    source = c("Google", "Google", "Google", "Seznam", "Google"),
    volume = c(10, 10, 20, 20, 30),
    cpc = c(1.0, 1.0, 2.0, 2.0, 3.0)
  )
  kwr <- kwr_import_mm("../test-data/", TRUE) |>
    kwresearch()
  expect_equal(kwr$sourceData, expected)
})
