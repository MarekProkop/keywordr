test_that("kwr_ngrams() works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  ngrams <- input_df |>
    kwresearch() |>
    kwr_ngrams()
  expect_s3_class(ngrams, "tbl_df")
  expect_named(ngrams, c("token", "volume", "n"))
  expect_equal(nrow(ngrams), 5)
})

test_that("kwr_subqueries() works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  subqueries <- input_df |>
    kwresearch() |>
    kwr_subqueries()
  expect_s3_class(subqueries, "tbl_df")
  expect_named(subqueries, c("token", "volume", "n"))
  expect_equal(nrow(subqueries), 2)
})

test_that("kwr_collocations() works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  coll <- input_df |>
    kwresearch() |>
    kwr_collocations()
  expect_s3_class(coll, "tbl_df")
  expect_named(coll, c("token", "volume", "n", "volume_prop", "n_prop"))
})
