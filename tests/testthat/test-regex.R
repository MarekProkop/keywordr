test_that("kwr_build_regex() replaces accents", {
  expect_equal(
    object = kwr_build_regex("vajíčko"),
    expected = "vaj[íi][čc]ko"
  )
})

test_that("kwr_build_regex() handles the and argument correctly", {
  expect_equal(
    object = kwr_build_regex("prší", "krásně"),
    expected = "kr[áa]sn[ěe].+pr[šs][íi]|pr[šs][íi].+kr[áa]sn[ěe]"
  )
})

test_that("kwr_test_regex() works", {
  input_df <- data.frame(
    query = c("aaa bbb ccc", "aaa bbb", "bbb ccc"),
    volume = c(100, 100, 100)
  )
  kwr <- input_df |>
    kwresearch()
  expect_type(kwr |> kwr_test_regex("aaa"), "list")
})
