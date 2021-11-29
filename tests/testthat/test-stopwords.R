test_that("kwr_stopwords('cs') works", {
  expect_type(kwr_stopwords("cs"), "character")
  expect_error(kwr_stopwords("en"))
})
