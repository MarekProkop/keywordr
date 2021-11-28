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
  expect_true(nrow(ngrams) == 6)
})
