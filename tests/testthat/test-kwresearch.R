test_that("kwresearch() creates an empty kwresearch object", {
  kwr <- kwresearch()
  expect_type(kwr, "list")
  expect_s3_class(kwr, "kwresearch")
  expect_length(kwr, 2)
  expect_named(kwr, c("status", "options"))
  expect_equal(kwr$status, "empty")
  expect_true(kwr$options$accentize)
  expect_true(kwr$options$normalize)
  expect_null(kwr$source_data)
  expect_null(kwr$clean_data)
  expect_null(kwr$classified_data)
  expect_null(kwr$recipes)
  expect_null(kwr$stopwords)
})

test_that("kwr_import() works", {
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
  kwr <- kwresearch() |>
    kwr_import(df_minimal)
  expect_equal(kwr$source_data, df_minimal_expected)
  expect_equal(kwr$status, "data")
})

test_that("kwresearch(df) cretaes an object and imports an initial data set", {
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
  expect_equal(kwr$source_data, df_minimal_expected)
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
  kwr <- kwresearch() |>
    kwr_import_mm("../test-data/", TRUE)
  expect_equal(kwr$source_data, expected)
})

test_that("accentize_queries() works as expected", {
  input_query <- c("a", "á", "c", "č")
  input_volume <- c(10, 100, 100, 1)
  output <- c("á", "á", "c", "c")
  expect_equal(accentize_queries(input_query, input_volume), output)
})

test_that("normalize_queries() works as axpected", {
  input_query <- c("a b", "b a", "c d", "d c")
  input_volume <- c(100, 10, 10, 100)
  output <- c("a b", "a b", "d c", "d c")
  expect_equal(normalize_queries(input_query, input_volume), output)
})

test_that("aggregate_clean_data() works as expected", {
  input <- tibble::tibble(
    query = c("a", "b", "c", "a", "b", "a"),
    query_normalized = c("a", "b", "c", "a", "b", "a"),
    volume = c(10, 20, 30, 40, 50, 10),
    cpc = c(1, 2, 3, 4, 5, 6),
    input = c("i1", "i1", "i1", "i1", "i1", "i2"),
    source = c("x", "x", "x", "y", "y", "x")
  )
  output <- tibble::tibble(
    query_normalized = c("b", "a", "c"),
    n_queries = c(2L, 3L, 1L),
    volume = c(70, 60, 30),
    cpc = c(5, 6, 3),
    query_original = c("b", "a", "c"),
    input = c("i1", "i1,i2", "i1"),
    source = c("x,y", "x,y", "x")
  )
  expect_equal(aggregate_clean_data(input), output)
})

test_that("clean_source_data() works as expected", {
  input <- tibble::tibble(
    query = c("a", "á", "b", "c", "a", "b"),
    volume = rep_len(10, 6),
    cpc = rep_len(1, 6),
    input = c(rep_len("i1", 5), "i2"),
    source = c(rep_len("x", 4), rep_len("y", 2))
  )
  output <- tibble::tibble(
    query_normalized = c("á", "b", "c"),
    n_queries = c(3L, 2L, 1L),
    volume = c(30, 20, 10),
    cpc = rep_len(1, 3),
    query_original = c("a,á", "b", "c"),
    input = c("i1", "i1,i2", "i1"),
    source = c("x,y", "x,y", "x")
  )
  expect_identical(clean_source_data(input), output)
})

test_that("kwr_source_queries() returns a correct data set", {
  input_df <- data.frame(
    query = c("seo", "seo", "keyword research"),
    volume = c(1000, 900, 500)
  )
  expected_df <- tibble::tibble(
    query = c("keyword research", "seo"),
    input = NA_character_,
    source = NA_character_,
    volume = c(500, 900),
    cpc = NA_real_
  )
  expect_equal(kwresearch(input_df) |> kwr_source_queries(), expected_df)
})

test_that("kwr_clean_queries() returns a correct data set", {
  input_df <- data.frame(
    query = c("seo", "seo", "keyword research"),
    volume = c(1000, 900, 500)
  )
  expected_df <- tibble::tibble(
    query_normalized = c("seo", "keyword research"),
    n_queries = c(1, 1),
    volume = c(900, 500),
    cpc = NA_real_,
    query_original = c("seo", "keyword research"),
    input = NA_character_,
    source = NA_character_
  )
  expect_equal(kwresearch(input_df) |> kwr_clean_queries(), expected_df)
})

test_that("kwr_classified_queries() returns a correct data set (or error)", {
  tibble_in <- tibble::tibble(
    query = c("aaa", "bbb", "ccc", "ddd"),
    volume = c(10, 20, 30, 40)
  )
  tibble_expected <- tibble::tibble(
    query_normalized = c("ddd", "ccc", "bbb", "aaa"),
    flag = c(FALSE, TRUE, FALSE, TRUE),
    label_1 = c("ddd", NA_character_, NA_character_, "aaa"),
    label_2 = c(NA_character_, "B/C", "B/C", "A"),
    n_queries = 1,
    volume = c(40, 30, 20, 10),
    cpc = NA_real_,
    query_original = c("ddd", "ccc", "bbb", "aaa"),
    input = NA_character_,
    source = NA_character_
  )
  kwr <- kwresearch(tibble_in)
  expect_error(kwr |> kwr_classified_queries())
  expect_equal(
    kwr |>
      kwr_classify(recipe_file = "../test-data/recipes-1.yml", quiet = TRUE) |>
      kwr_classified_queries(),
    tibble_expected
  )
})

test_that("kwr_pruned_queries() return a correct data set", {
  expect_equal(
    object = kwresearch(tibble::tibble(
      query = c("aaa", "bbb", "ccc"),
      volume = 10
    )) |>
      kwr_prune("../test-data/recipes-prune-1.yml", quiet = TRUE) |>
      kwr_removed_queries(),
    expected = tibble::tibble(
      query_normalized = c("aaa", "ccc"),
      n_queries = 1,
      volume = 10,
      cpc = NA_real_,
      query_original = c("aaa", "ccc"),
      input = NA_character_,
      source = NA_character_
    )
  )
})

test_that("kwr_use_stopwords() works", {
  kwr <- kwresearch() |>
    kwr_use_stopwords(kwr_stopwords())
  expect_type(kwr$stopwords, "character")
})

test_that("dimension_names() works", {
  expect_equal(
    object = dimension_names(tibble::tibble(
      query_normalized = "x",
      a = "a",
      b = "b",
      n_queries = 1,
      volume = 10,
      cpc = 1,
      query_original = "a",
      input = "a",
      source = "a"
    )),
    expected = c("a", "b")
  )
})

test_that("is_unclassified() works", {
  expect_true(is_unclassified(NA_character_))
  expect_true(is_unclassified(FALSE))
  expect_false(is_unclassified("a"))
  expect_false(is_unclassified(TRUE))
})

test_that("kwr_unclassified_queries() works", {
  tibble_in <- tibble::tibble(
    query = c("aaa", "xxx"),
    volume = c(10, 20)
  )
  tibble_expected <- tibble::tibble(
    query_normalized = c("xxx"),
    n_queries = 1,
    volume = c(20),
    cpc = NA_real_,
    query_original = c("xxx"),
    input = NA_character_,
    source = NA_character_
  )
  result <- kwresearch(tibble_in) |>
    kwr_classify(recipe_file = "../test-data/recipes-1.yml", quiet = TRUE) |>
    kwr_unclassified_queries()
  expect_equal(result, tibble_expected)
})

