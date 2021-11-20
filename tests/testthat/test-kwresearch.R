test_that("kwresearch() creates an empty kwresearch object", {
  kwr <- kwresearch()
  expect_equal(kwr$status, "empty")
  expect_s3_class(kwr, "kwresearch")
})
