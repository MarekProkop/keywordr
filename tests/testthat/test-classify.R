test_that("join_patterns(x) joins regex patterns", {
  input <- c("aaa", "bbb", "aaa", "ccc")
  output <- "aaa|bbb|ccc"
  expect_equal(join_patterns(input), output)
})

test_that("extract_pattern() extracts a pattern correctly", {
  vect <- c("aaa", "bbb xxx", "ccc", "xxx ddd yyy")
  pattern <- "xxx|yyy"
  output <- c(NA_character_, "xxx", NA_character_, "xxx,yyy")

  expect_identical(extract_pattern(vect, pattern), output)
})

tibble_1 <- tibble::tibble(query_normalized = c("aaa", "bbb", "ccc", "ddd"))
tibble_2 <- tibble::tibble(
  query_normalized = c("aaa", "bbb", "ccc", "ddd"),
  flag = c(FALSE, TRUE, FALSE, TRUE)
)
tibble_3 <- tibble::tibble(
  query_normalized = c("aaa", "bbb", "ccc", "ddd"),
  flag = c(TRUE, FALSE, TRUE, FALSE)
)

test_that("set_flag() sets a flag correctly", {
  flad_name <- "flag"
  pattern <- "bbb|ddd"

  expect_equal(set_flag(tibble_1, flad_name, pattern), tibble_2)
  expect_equal(set_flag(tibble_1, flad_name, pattern, negate = TRUE), tibble_3)
})

test_that("process_recipe() sets a flag correctly", {
  recipe_1 <- list(
    type = "flag",
    name = "flag",
    negate = FALSE,
    patterns = c("bbb", "ddd")
  )
  recipe_2 <- recipe_1
  recipe_2$negate <- TRUE

  expect_identical(process_recipe(tibble_1, recipe_1), tibble_2)
  expect_identical(process_recipe(tibble_1, recipe_2), tibble_3)

  # Shoud work iven if a label already exists
  expect_identical(process_recipe(tibble_2, recipe_2), tibble_3)
})

tibble_4 <- tibble::tibble(
  query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy")
)
tibble_5 <- tibble::tibble(
  query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
  label = c(NA_character_, "xxx", NA_character_, "xxx,yyy")
)
tibble_6 <- tibble::tibble(
  query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
  label = c(NA_character_, "X", NA_character_, "X")
)
tibble_7 <- tibble::tibble(
  query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
  label = c(NA_character_, "X", NA_character_, "X,Y")
)

test_that("set_label() sets a new label without a predefined value", {
  expect_equal(set_label(tibble_4, "label", "xxx|yyy"), tibble_5)
})

test_that("set_label() sets a new label with a predefined value", {
  expect_equal(set_label(tibble_4, "label", "xxx", "X"), tibble_6)
})

test_that("set_label() sets an existing label with a predefined value", {
  expect_equal(set_label(tibble_6, "label", "yyy", "Y"), tibble_7)
})

test_that("process_recipe() sets a new label without a predefined value", {
  recipe <- list(
    type = "label",
    name = "label",
    patterns = c("xxx", "yyy")
  )
  expect_equal(process_recipe(tibble_4, recipe), tibble_5)
})

test_that("process_recipe() sets a new label with a predefined value", {
  recipe <- list(
    type = "label",
    name = "label",
    values = list(
      list(
        value = "X",
        patterns = "xxx"
      )
    )
  )
  expect_equal(process_recipe(tibble_4, recipe), tibble_6)
})

test_that("process_recipe() sets an existing label with a predefined value", {
  recipe <- list(
    type = "label",
    name = "label",
    values = list(
      list(
        value = "Y",
        patterns = "yyy"
      )
    )
  )
  expect_equal(process_recipe(tibble_6, recipe), tibble_7)
})

test_that("process_recipe() sets a new label with multiple predefined values", {
  recipe <- list(
    type = "label",
    name = "label",
    values = list(
      list(
        value = "X",
        patterns = "xxx"
      ),
      list(
        value = "Y",
        patterns = "yyy"
      )
    )
  )
  expect_equal(process_recipe(tibble_4, recipe), tibble_7)
})

test_that("process_recipe() handles multiple recipes correctly", {
  tibble_in <- tibble::tibble(
    query_normalized = c("aaa", "bbb", "ccc", "ddd"),
    volume = c(10, 20, 30, 40)
  )
  tibble_expected <- tibble::tibble(
    query_normalized = c("aaa", "bbb", "ccc", "ddd"),
    volume = c(10, 20, 30, 40),
    flag = c(TRUE, FALSE, TRUE, FALSE),
    label_1 = c("aaa", NA_character_, NA_character_, "ddd"),
    label_2 = c("A", "B/C", "B/C", NA_character_)
  )
  recipes <- list(
    list(
      type = "flag",
      name = "flag",
      negate = FALSE,
      patterns = c("aaa", "ccc")
    ),
    list(
      type = "label",
      name = "label_1",
      patterns = c("aaa", "ddd")
    ),
    list(
      type = "label",
      name = "label_2",
      values = list(
        list(
          value = "A",
          patterns = "aaa"
        ),
        list(
          value = "B/C",
          patterns = c("bbb", "ccc")
        )
      )
    )
  )
  tibble_out <- recipes |> purrr::reduce(process_recipe, .init = tibble_in)
  expect_equal(tibble_out, tibble_expected)
})

test_that("kwr_classify() works as expected", {
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
  kwr <- kwresearch(tibble_in) |>
    kwr_classify("../test-data/recipes.yml")
  expect_equal(kwr$classified_data, tibble_expected)
})
