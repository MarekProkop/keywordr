test_that("kwr_add_pattern() wth wrong arguments fails", {
  testthat::expect_error(kwr_add_pattern(
    pattern = NULL,
    recipe_file = NULL,
    recipe_type = NULL,
    dim_name = NULL,
    value = NULL
  ), "pattern")
  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = NULL,
    recipe_type = NULL,
    dim_name = NULL,
    value = NULL
  ), "recipe_file")
  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = NULL,
    dim_name = NULL,
    value = NULL
  ), "recipe_type")

  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "xxx",
    dim_name = NULL,
    value = NULL
  ), "recipe_type")

  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "remove",
    dim_name = "xxx",
    value = NULL
  ), "dim_name")
  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "remove",
    dim_name = NULL,
    value = "xxx"
  ), "value")
  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "include",
    dim_name = "xxx",
    value = NULL
  ), "dim_name")
  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "include",
    dim_name = NULL,
    value = "xxx"
  ), "value")

  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "label",
    dim_name = NULL,
    value = NULL
  ), "dim_name")

  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "flag",
    dim_name = NULL,
    value = NULL
  ), "dim_name")
  testthat::expect_error(kwr_add_pattern(
    pattern = "a",
    recipe_file = file.path(tempdir(), "test.yml"),
    recipe_type = "flag",
    dim_name = "xxx",
    value = "xxx"
  ), "value")
})

test_that("build_recipe() works with type = 'remove'", {
  expect_equal(
    object = build_recipe(recipe_type = "remove", pattern = "a"),
    expected = list(list(type = "remove", rules = list(list(match = "a"))))
  )
})

test_that("build_recipe() works with type = 'include'", {
  expect_equal(
    object = build_recipe(recipe_type = "include", pattern = "a"),
    expected = list(list(type = "include", rules = list(list(match = "a"))))
  )
})

test_that("build_recipe() works with type = 'label' without value", {
  expect_equal(
    object = build_recipe(
      recipe_type = "label", dim_name = "name_1", pattern = "a"
    ),
    expected = list(
      list(type = "label", name = "name_1", rules = list(list(match = "a")))
    )
  )
})

test_that("build_recipe() works with type = 'label' and a value", {
  expect_equal(
    object = build_recipe(
      recipe_type = "label",
      dim_name = "name_1",
      value = "value_1",
      pattern = "a"
    ),
    expected = list(
      list(
        type = "label",
        name = "name_1",
        values = list(list(value = "value_1", rules = list(list(match = "a"))))
      )
    )
  )
})

recipe_dir <- tempdir()

test_that("kwr_add_pattern() creates a new yaml", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  if (file.exists(recipe_file)) {
    file.remove(recipe_file)
  }
  kwr_add_pattern(
    pattern = "a", recipe_file = recipe_file, recipe_type = "remove"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(list(type = "remove", rules = list(list(match = "a"))))
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds a new 'remove' recipe to an existing yaml", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "label", name = "name_1", rules = list(list(match = "a")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "b",
    recipe_file = recipe_file,
    recipe_type = "remove"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "label", name = "name_1", rules = list(list(match = "a"))),
      list(type = "remove", rules = list(list(match = "b")))
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds a new pattern to the 'remove' recipe", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "remove", rules = list(list(match = c("a", "b"))))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "c",
    recipe_file = recipe_file,
    recipe_type = "remove"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "remove", rules = list(list(match = c("a", "b", "c"))))
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() does not add a duplicate pattern to the 'remove' recipe", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "remove", rules = list(list(match = c("a", "b"))))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "b",
    recipe_file = recipe_file,
    recipe_type = "remove"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "remove", rules = list(list(match = c("a", "b"))))
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds the 1st label to an existing yaml", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "remove", rules = list(list(match = "a")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "b",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "remove", rules = list(list(match = "a"))),
      list(type = "label", name = "name_1", rules = list(list(match = "b")))
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds another label to an existing yaml", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "remove", rules = list(list(match = "a"))),
      list(type = "label", name = "name_1", rules = list(list(match = "b")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "c",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_2"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "remove", rules = list(list(match = "a"))),
      list(type = "label", name = "name_1", rules = list(list(match = "b"))),
      list(type = "label", name = "name_2", rules = list(list(match = "c")))
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds a new pattern to an existing label", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "label", name = "name_1", rules = list(list(match = "a")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "b",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "label", name = "name_1", rules = list(list(match = c("a", "b"))))
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() does not add a duplicate pattern to an existing label", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "label", name = "name_1", rules = list(list(match = "a")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "a",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "label", name = "name_1", rules = list(list(match = "a")))
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds the 1st label and value to an existing yaml", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "remove", rules = list(list(match = "a")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "b",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1",
    value = "B"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "remove", rules = list(list(match = "a"))),
      list(
        type = "label",
        name = "name_1",
        values = list(list(value = "B", rules = list(list(match = "b"))))
      )
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds another label and value to an existing yaml", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "remove", rules = list(list(match = "a"))),
      list(type = "label", name = "name_1", rules = list(list(match = "b")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "c",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_2",
    value = "C"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(type = "remove", rules = list(list(match = "a"))),
      list(type = "label", name = "name_1", rules = list(list(match = "b"))),
      list(
        type = "label",
        name = "name_2",
        values = list(
          list(
            value = "C",
            rules = list(list(match = "c"))
          )
        )
      )
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds the 1st value to an existing label", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(type = "label", name = "name_1", rules = list(list(match = "a")))
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "b",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1",
    value = "value_1"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(
        type = "label",
        name = "name_1",
        rules = list(list(match = "a")),
        values = list(
          list(
            value = "value_1",
            rules = list(list(match = "b"))
          )
        )
      )
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds the 1st value to an existing label (complex)", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(
        type = "label",
        name = "name_1",
        rules = list(list(match = c("a", "b", "c")))
      )
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "po[šs]t[aeuy]\\b",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1",
    value = "pošta"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(
        type = "label",
        name = "name_1",
        rules = list(list(match = c("a", "b", "c"))),
        values = list(
          list(
            value = "pošta",
            rules = list(list(match = "po[šs]t[aeuy]\\b"))
          )
        )
      )
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds another value to an existing label", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(
        type = "label",
        name = "name_1",
        rules = list(list(match = "a")),
        values = list(
          list(
            value = "value_1",
            rules = list(list(match = "b"))
          )
        )
      )
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "c",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1",
    value = "value_2"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(
        type = "label",
        name = "name_1",
        rules = list(list(match = "a")),
        values = list(
          list(
            value = "value_1",
            rules = list(list(match = "b"))
          ),
          list(
            value = "value_2",
            rules = list(list(match = "c"))
          )
        )
      )
    )
  )
  file.remove(recipe_file)
})

test_that("kwr_add_pattern() adds a new pattern to an existing label and value", {
  recipe_file <- file.path(recipe_dir, "test.yml")
  yaml::write_yaml(
    list(
      list(
        type = "label",
        name = "name_1",
        values = list(
          list(
            value = "value_1",
            rules = list(list(match = "a"))
          )
        )
      )
    ),
    file = recipe_file
  )
  kwr_add_pattern(
    pattern = "b",
    recipe_file = recipe_file,
    recipe_type = "label",
    dim_name = "name_1",
    value = "value_1"
  )
  expect_equal(
    object = yaml::read_yaml(recipe_file),
    expected = list(
      list(
        type = "label",
        name = "name_1",
        values = list(
          list(
            value = "value_1",
            rules = list(list(match = c("a", "b")))
          )
        )
      )
    )
  )
  file.remove(recipe_file)
})

