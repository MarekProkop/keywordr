test_that("kwr_import_mm(file) imports a sigle file", {
  expected <- tibble::tibble(
    query = c("query 1", "query 2"),
    input = c("input 1"),
    source = c("Google"),
    volume = c(10, 20),
    cpc = c(1.0, 2.0)
  )
  kwr <- kwresearch() |> kwr_import_mm("../test-data/mm-test-1.csv", TRUE)
  expect_equal(kwr$source_data, expected)
})

test_that("kwr_import_mm(files) imports multiple files", {
  expected <- tibble::tibble(
    query = c("query 1", "query 1", "query 2", "query 2", "query 3"),
    input = c("input 1", "input 2", "input 1", "input 1", "input 2"),
    source = c("Google", "Google", "Google", "Seznam", "Google"),
    volume = c(10, 10, 20, 20, 30),
    cpc = c(1.0, 1.0, 2.0, 2.0, 3.0)
  )
  kwr <- kwresearch() |> kwr_import_mm("../test-data/", TRUE)
  expect_equal(kwr$source_data, expected)

  kwr <- kwresearch() |> kwr_import_mm(
    c("../test-data/mm-test-1.csv", "../test-data/mm-test-2.csv"),
    quiet = TRUE
  )
  expect_equal(kwr$source_data, expected)
})
