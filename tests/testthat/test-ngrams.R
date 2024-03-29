test_that("remove_nested_ngrams() works", {
  expect_equal(
    remove_nested_ngrams(
      tibble::tibble(
        token = c("a", "b", "b c", "d", "e d", "d e f"),
        n = c(5, 4, 3, 2, 2, 1),
        volume = c(50, 40, 30, 20, 20, 10)
      ), 3
    ),
    tibble::tibble(
      token = c("a", "b", "b c", "e d", "d e f"),
      n = c(5, 4, 3, 2, 1),
      volume = c(50, 40, 30, 20, 10)
    )
  )
})

test_that("kwr_ngrams(remove_nested = FALSE) works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  expected <- tibble::tibble(
    token = c("bbb", "aaa", "aaa bbb", "bbb ccc", "ccc", "aaa bbb ccc"),
    n = c(3, 2, 2, 2, 2, 1),
    volume = c(300, 200, 200, 200, 200, 100)
  )
  ngrams <- input_df |>
    kwresearch() |>
    kwr_ngrams(remove_nested = FALSE)
  expect_s3_class(ngrams, "tbl_df")
  expect_named(ngrams, c("token", "n", "volume"))
  expect_equal(nrow(ngrams), 6)
  expect_equal(ngrams, expected)
  expect_equal(
    object = input_df |>
      kwresearch() |>
      kwr_queries() |>
      kwr_ngrams(remove_nested = FALSE),
    expected = expected
  )
})

test_that("kwr_ngrams(remove_nested = TRUE) works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  expected <- tibble::tibble(
    token = c("bbb", "aaa bbb", "bbb ccc", "aaa bbb ccc"),
    n = c(3, 2, 2, 1),
    volume = c(300, 200, 200, 100)
  )
  expect_equal(
    input_df |> kwresearch() |>
      kwr_ngrams(remove_nested = TRUE),
    expected
  )
})

test_that("kwr_ngrams() with stopwords works", {
  input_df <- data.frame(
    query = c("aaa bbb", "aaa že bbb"),
    volume = c(100, 100)
  )
  expected <- tibble::tibble(
    token = c("aaa", "bbb", "aaa bbb", "aaa že", "aaa že bbb", "že bbb"),
    n = c(2, 2, 1, 1, 1, 1),
    volume = c(200, 200, 100, 100, 100, 100)
  )
  expect_equal(
    input_df |> kwresearch() |>
      kwr_use_stopwords(kwr_stopwords(lang = "cs")) |>
      kwr_ngrams(remove_nested = FALSE),
    expected
  )
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
  expect_named(subqueries, c("token", "n", "volume"))
  expect_equal(nrow(subqueries), 2)
  expect_equal(
    subqueries,
    tibble::tibble(
      token = c("aaa bbb", "bbb ccc"),
      n = c(1, 1),
      volume = c(100, 100)
    )
  )
  expect_equal(
    object = input_df |>
      kwresearch() |>
      kwr_queries() |>
      kwr_subqueries(),
    expected = tibble::tibble(
      token = c("aaa bbb", "bbb ccc"),
      n = c(1, 1),
      volume = c(100, 100)
    )
  )
})

test_that("kwr_collocations() works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  coll <- input_df |>
    kwresearch() |>
    kwr_collocations(quanteda = FALSE)
  expect_s3_class(coll, "tbl_df")
  expect_named(coll, c("token", "n", "volume", "n_prop", "volume_prop"))
  expect_s3_class(
    object = input_df |>
      kwresearch() |>
      kwr_queries() |>
      kwr_collocations(quanteda = FALSE),
    class = "tbl_df"
  )
})
