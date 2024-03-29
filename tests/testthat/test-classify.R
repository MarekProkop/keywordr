test_that("kwr_use_recipes() adds a recipe file to an empty kwresearch", {
  skip("deprecated")
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
  skip("deprecated")
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

test_that("extract_pattern() extracts a pattern without a capture group", {
  expect_identical(
    object = extract_pattern(
      x = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
      pattern = "xxx|yyy"
    ),
    expected = c(NA_character_, "xxx", NA_character_, "xxx,yyy")
  )
})

test_that("extract_pattern() extracts a pattern with a capture group", {
  expect_identical(
    object = extract_pattern(
      x = c("abc", "aabcc", "cba", "abc adc"),
      pattern = "a(b|d)c"
    ),
    expected = c("b", "b", NA_character_, "b,d")
  )
  expect_identical(
    object = extract_pattern(
      x = c("abc x", "x abc", "x", "def x", "abc y"),
      pattern = "abc.+(x)|(x).+abc"
    ),
    expected = c("x", "x", NA_character_, NA_character_, NA_character_)
  )
  expect_identical(
    object = extract_pattern(
      x = c("aa", "bbxx", "cc", "dd"),
      pattern = "bb(xx)|dd"
    ),
    expected = c(NA_character_, "xx", NA_character_, "dd")
  )
  expect_identical(
    object = extract_pattern(
      x = c("aa", "bbxx", "ccyy", "dd"),
      pattern = "bb(xx)|cc(yy)|dd"
    ),
    expected = c(NA_character_, "xx", "yy", "dd")
  )
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
    rules = list(list(match = c("bbb", "ddd")))
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

test_that("set_label() sets a label with capture group", {
  queries <- c("aaa", "bbb", "ccc", "ddd")
  expect_identical(
    object = set_label(
      df = tibble::tibble(
        query_normalized = queries
      ),
      name = "label_name",
      pattern = "b(bb)"
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb", "ccc", "ddd"),
      label_name = c(NA_character_, "bb", NA_character_, NA_character_)
    )
  )
  expect_identical(
    object = set_label(
      df = tibble::tibble(
        query_normalized = queries
      ),
      name = "label_name",
      pattern = "b(bb)|ddd"
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb", "ccc", "ddd"),
      label_name = c(NA_character_, "bb", NA_character_, "ddd")
    )
  )
})

test_that("process_label_rule() works with the most simple rule", {
  queries <- c("aaa", "bbb", "ccc", "ddd")
  expect_identical(
    object = process_label_rule(
      df = tibble::tibble(
        query_normalized = queries
      ),
      rule = list(match = c("bbb", "ddd")),
      name = "label_name"
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb", "ccc", "ddd"),
      label_name = c(NA_character_, "bbb", NA_character_, "ddd")
    )
  )
})

test_that("process_label_rule() works with a value", {
  queries <- c("aaa", "bbb", "ccc", "ddd")
  expect_identical(
    object = process_label_rule(
      df = tibble::tibble(
        query_normalized = queries
      ),
      rule = list(match = c("bbb", "ddd")),
      name = "label_name",
      value = "label_value"
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb", "ccc", "ddd"),
      label_name = c(NA_character_, "label_value", NA_character_, "label_value")
    )
  )
})

test_that("process_label_rule() works with a capture group", {
  queries <- c("aaa", "bbb", "ccc", "ddd")
  expect_identical(
    object = process_label_rule(
      df = tibble::tibble(
        query_normalized = queries
      ),
      rule = list(match = c("b(bb)", "ddd")),
      name = "label_name"
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb", "ccc", "ddd"),
      label_name = c(NA_character_, "bb", NA_character_, "ddd")
    )
  )
})

test_that("process_recipe() sets a new label without a predefined value", {
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy")
      ),
      recipe = list(
        type = "label",
        name = "label",
        rules = list(list(match = c("xxx", "yyy")))
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
      label = c(NA_character_, "xxx", NA_character_, "xxx,yyy")
    )
  )
})

test_that("process_recipe() sets a new label with a predefined value", {
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy")
      ),
      recipe = list(
        type = "label",
        name = "label",
        values = list(list(
          value = "X",
          rules = list(list(match = "xxx"))
        ))
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
      label = c(NA_character_, "X", NA_character_, "X")
    )
  )
})

test_that("process_recipe() sets an existing label with a predefined value", {
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
        label = c(NA_character_, "X", NA_character_, "X")
      ),
      recipe = list(
        type = "label",
        name = "label",
        values = list(list(
          value = "Y",
          rules = list(list(match = "yyy"))
        ))
      ),
      quiet = TRUE),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
      label = c(NA_character_, "X", NA_character_, "X,Y")
    )
  )
})

test_that("process_recipe() sets a new label with multiple predefined values", {
  recipe <-
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy")
      ),
      recipe = list(
        type = "label",
        name = "label",
        values = list(
          list(
            value = "X",
            rules = list(list(match = "xxx"))
          ),
          list(
            value = "Y",
            rules = list(list(match = "yyy"))
          )
        )
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb xxx", "ccc", "xxx ddd yyy"),
      label = c(NA_character_, "X", NA_character_, "X,Y")
    )
  )
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
        values = list(list(
          value = "A",
          rules = list(list(
            match = "aaa",
            except = "ccc"
          ))
        ))
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa", "bbb", "aaa bbb", "aaa ccc", "aaa ddd"),
      label = c("A", NA_character_, "A", NA_character_, "A")
    )
  )
})

test_that("process_recipe() handles values and rules in the same recipe", {
  queries <- c("aaa", "bbb", "ccc")

  # Values followed by rules
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = queries
      ),
      recipe = list(
        type = "label",
        name = "label",
        values = list(list(
          value = "A",
          rules = list(list(match = "aaa"))
        )),
        rules = list(list(match = "bbb"))
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = queries,
      label = c("A", "bbb", NA_character_)
    )
  )
  # Rules followed by values
  expect_equal(
    object = process_recipe(
      df = tibble::tibble(
        query_normalized = queries
      ),
      recipe = list(
        type = "label",
        name = "label",
        rules = list(list(match = "bbb")),
        values = list(list(
          value = "A",
          rules = list(list(match = "aaa"))
        ))
      ),
      quiet = TRUE
    ),
    expected = tibble::tibble(
      query_normalized = queries,
      label = c("A", "bbb", NA_character_)
    )
  )
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
      rules = list(list(match = c("aaa", "ccc")))
    ),
    list(
      type = "label",
      name = "label_1",
      rules = list(list(match = c("aaa", "ddd")))
    ),
    list(
      type = "label",
      name = "label_2",
      values = list(
        list(
          value = "A",
          rules = list(list(match = "aaa"))
        ),
        list(
          value = "B/C",
          rules = list(list(match = c("bbb", "ccc")))
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
    kwr_classify(recipe_file = "../test-data/recipes-1.yml", quiet = TRUE)
  expect_equal(kwr$classified_data, tibble_expected)
})
