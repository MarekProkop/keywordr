test_that("kwr_stopwords('cs') works", {
  expect_type(kwr_stopwords("cs"), "character")
  expect_error(kwr_stopwords("en"))
})

test_that("kwr_remove_stopwords() works", {
  expect_equal(
    object = tibble::tibble(
      token = c("a", "b", "c", "d", "e"),
      n = c(1, 2, 3, 4, 5)
    ) |>
      kwr_remove_stopwords(c("b", "c", "e")),
    expected = tibble::tibble(
      token = c("a", "d"),
      n = c(1, 4)
    )
  )
})
