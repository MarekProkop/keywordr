#' Explore a regex pattern applied on queries
#'
#' @param kwr A kwresearch object.
#' @param pattern A single regular experession.
#'
#' @return A list of data frames.
#' @export
kwr_test_regex <- function(kwr, pattern) {
  full <- kwr |>
    kwr_clean_queries() |>
    dplyr::select(.data$query_normalized) |>
    dplyr::filter(stringr::str_detect(.data$query_normalized, pattern)) |>
    dplyr::mutate(
      match = stringr::str_extract(.data$query_normalized, pattern),
      word = stringr::str_extract(.data$query_normalized, stringr::str_c("[\\B]*?", pattern, ".*?\\b")),
      schema = stringr::str_replace(.data$query_normalized, stringr::str_c("[\\B]*?", pattern, ".*?\\b"), "*"),
      pred = stringr::str_replace(stringr::str_replace(.data$schema, " \\*.*", ""), ".*\\*.*", ""),
      succ = stringr::str_replace(stringr::str_replace(.data$schema, ".*\\* ", ""), ".*\\*.*", "")
    ) |>
    dplyr::select(!.data$schema)
  around <- full |>
    tidyr::pivot_longer(cols = c(.data$pred, .data$succ), values_to = "around") |>
    dplyr::filter(.data$around != "") |>
    dplyr::count(.data$around, sort = TRUE)
  around_ngrams <- around |>
    tidytext::unnest_ngrams("token", "around", n = 4, n_min = 1) |>
    dplyr::count(.data$token, sort = TRUE)
  if (!is.null(kwr$stopwords)) {
    around_ngrams <- around_ngrams |>
      dplyr::filter(!.data$token %in% kwr$stopwords)
  }
  list(
    full = full,
    words = full |> dplyr::count(.data$word, sort = TRUE),
    around = around,
    around_ngrams = around_ngrams
  )
}
