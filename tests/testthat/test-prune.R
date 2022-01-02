test_that("process_prune_recipe() removes correct queries", {

  # Full syntax, a single rule without except
  expect_equal(
    object = process_prune_recipe(
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
      )
    ),
    expected = tibble::tibble(
      query_normalized = c("bbb", "ddd")
    )
  )

  # Full syntax, multiple rules without except
  expect_equal(
    object = process_prune_recipe(
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
      )
    ),
    expected = tibble::tibble(
      query_normalized = c("bbb")
    )
  )

  # Full syntax, a single rule with except
  expect_equal(
    object = process_prune_recipe(
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
      )
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa iii", "bbb iii")
    )
  )

  # Full syntax, multiple rules with except
  expect_equal(
    object = process_prune_recipe(
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
      )
    ),
    expected = tibble::tibble(
      query_normalized = c("aaa iii", "bbb jjj")
    )
  )

})

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

test_that("kwr_queries() returns pruned data if available", {
  kwr <- kwresearch(
    queries = tibble::tibble(
      query = c("aaa", "bbb", "ccc", "ccc xxx", "ddd"),
      volume = c(10, 20, 30, 40, 50)
    )
  ) |> kwr_prune("../test-data/recipes-prune-1.yml", quiet = TRUE)

  expect_equal(kwr_queries(kwr), kwr$pruned_data)
})
