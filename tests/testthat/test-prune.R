test_that("kwr_prune() prunes clean data", {
  kwr <- kwresearch(
    queries = tibble::tibble(
      query = c("aaa", "bbb", "ccc", "ccc xxx", "ddd"),
      volume = c(10, 20, 30, 40, 50)
    )
  ) |> kwr_prune("../test-data/recipes-prune-1.yml", quiet = TRUE)

  expect_equal(kwr$status, "pruned")
  expect_s3_class(kwr$pruned_data, "tbl_df")
  expect_equal(
    kwr$pruned_data,
    tibble::tibble(
      query_normalized = c("ddd", "ccc xxx", "bbb"),
      n_queries = 1,
      volume = c(50, 40, 20),
      cpc = NA_real_,
      query_original = c("ddd", "ccc xxx", "bbb"),
      input = NA_character_,
      source = NA_character_
    )
  )
})
