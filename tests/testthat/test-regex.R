test_that("kwr_test_regex() works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  kwr <- input_df |>
    kwresearch()
  expect_type(kwr |> kwr_test_regex("aaa"), "list")
})
