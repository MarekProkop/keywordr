test_that("kwr_use_recipes() adds a recipe file to an empty kwresearch", {
  expected_recipes <- list(
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
  kwr <- kwresearch() |> kwr_use_recipes("../test-data/recipes-1.yml")
  expect_type(kwr$recipes, "list")
  expect_equal(kwr$recipes, expected_recipes)
})

test_that("kwr_use_recipes() adds 2 recipe files to an empty kwresearch", {
  expected_recipes <- list(
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
    ),
    list(
      type = "label",
      name = "label_3",
      patterns = "eee"
    )
  )
  kwr <- kwresearch() |>
    kwr_use_recipes("../test-data/recipes-1.yml") |>
    kwr_use_recipes("../test-data/recipes-2.yml")
  expect_equal(kwr$recipes, expected_recipes)
})

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

test_that("process_recipe() removes correct queries", {

  # Full syntax, a single rule without except
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa", "bbb", "ccc", "ddd")
      ),
      recipe = list(
        type = "remove",
        rules = list(
          list(
            match = c("aaa", "ccc")
          )
        )
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("bbb", "ddd")
    )
  )

  # Full syntax, multiple rules without except
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa", "bbb", "ccc", "ddd")
      ),
      recipe = list(
        type = "remove",
        rules = list(
          list(
            match = c("aaa", "ccc")
          ),
          list(
            match = c("aaa", "ddd")
          )
        )
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("bbb")
    )
  )

  # Full syntax, a single rule with except
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa iii", "bbb iii", "bbb jjj")
      ),
      recipe = list(
        type = "remove",
        rules = list(
          list(
            match = "bbb",
            except = "iii"
          )
        )
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa iii", "bbb iii")
    )
  )

  # Full syntax, multiple rules with except
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa iii", "aaa jjj", "bbb iii", "bbb jjj")
      ),
      recipe = list(
        type = "remove",
        rules = list(
          list(
            match = "aaa",
            except = "iii"
          ),
          list(
            match = "bbb",
            except = "jjj"
          )
        )
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa iii", "bbb jjj")
    )
  )

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

  expect_identical(process_recipe(tibble_1, recipe_1, quiet = TRUE), tibble_2)
  expect_identical(process_recipe(tibble_1, recipe_2, quiet = TRUE), tibble_3)

  # Shoud work iven if a label already exists
  expect_identical(process_recipe(tibble_2, recipe_2, quiet = TRUE), tibble_3)
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

test_that("set_label() does not set a label for an excluded pattern", {
  queries <- c("aaa", "bbb", "aaa bbb", "aaa ccc", "aaa ddd")
  inp <- tibble::tibble(
    query_normalized = queries
  )
  out <- tibble::tibble(
    query_normalized = queries,
    label = c("A", NA_character_, "A", NA_character_, "A")
  )

  expect_equal(
    object = set_label(
      df = inp, name = "label", pattern = "aaa", value = "A", exclude = "ccc"
    ),
    expected = tibble::tibble(
      query_normalized = queries,
      label = c("A", NA_character_, "A", NA_character_, "A")
    )
  )
  expect_equal(
    object = set_label(
      df = inp, name = "label", pattern = "aaa", value = NULL, exclude = "ccc"
    ),
    expected = tibble::tibble(
      query_normalized = queries,
      label = c("aaa", NA_character_, "aaa", NA_character_, "aaa")
    )
  )
  expect_equal(
    set_label(
      tibble::tibble(
        query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy")
      ),
      name = "label", pattern = "xxx", value = "X", exclude = "yyy"
    ),
    tibble::tibble(
      query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
      label = c(NA_character_, "X", NA_character_, NA_character_)
    )
  )
})

test_that("process_recipe() sets a new label without a predefined value", {
  recipe <- list(
    type = "label",
    name = "label",
    patterns = c("xxx", "yyy")
  )
  expect_equal(process_recipe(tibble_4, recipe, quiet = TRUE), tibble_5)
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
  expect_equal(process_recipe(tibble_4, recipe, quiet = TRUE), tibble_6)
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
  expect_equal(process_recipe(tibble_6, recipe, quiet = TRUE), tibble_7)
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
  expect_equal(process_recipe(tibble_4, recipe, quiet = TRUE), tibble_7)
})

test_that("process_recipe() doesn't set a label for an excluded patterns", {
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa", "bbb", "aaa bbb", "aaa ccc", "aaa ddd")
      ),
      recipe = list(
        type = "label",
        name = "label",
        values = list(
          list(
            value = "A",
            patterns = "aaa",
            exclude = "ccc"
          )
        )
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb", "aaa bbb", "aaa ccc", "aaa ddd"),
      label = c("A", NA_character_, "A", NA_character_, "A")
    )
  )
})

test_that("process_recipe() handles values and patterns in the same recipe", {
  queries <- c("aaa", "bbb", "ccc")

  # Values followed by patterns
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = queries
      ),
      recipe = list(
        type = "label",
        name = "label",
        values = list(
          list(
            value = "A",
            patterns = "aaa"
          )
        ),
        patterns = "bbb"
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = queries,
      label = c("A", "bbb", NA_character_)
    )
  )
  # Patterns followed by values
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = queries
      ),
      recipe = list(
        type = "label",
        name = "label",
        patterns = "bbb",
        values = list(
          list(
            value = "A",
            patterns = "aaa"
          )
        )
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = queries,
      label = c("A", "bbb", NA_character_)
    )
  )
})

test_that("process_recipe() doesn't set a label for an excluded patterns", {
  skip("to do")
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
  tibble_out <- recipes |>
    purrr::reduce(process_recipe, .init = tibble_in, quiet = TRUE)
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
    kwr_use_recipes("../test-data/recipes-1.yml") |>
    kwr_classify(quiet = TRUE)
  expect_equal(kwr$classified_data, tibble_expected)
})
