#' Generate n-grams from queries
#'
#' @param kwr A kwresearch object, which clean queries will be n-grams
#'   calculated from.
#' @param max_words Maximum number of words in n-grams.
#' @param min_words Minimum number of words in n-grams.
#' @param min_n Minimum number of queries. Only the n-grams with at least this
#'   number of queries will be included.
#' @param min_volume Minimum search volume per n-gram. Only the n-grams with at
#'   least this volume will be included.
#'
#' @return A tibble of n-grams with a basic stats (nuber of queries and sum of
#'   search volumes). The n-grams are orderd descendingly by number of queries
#'   and search volume. Use dplyr::arrange to change order.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#' kwr |> kwr_ngrams()
kwr_ngrams <- function(
  kwr, max_words = 4, min_words = 1, min_n = 1, min_volume = 1
) {
  kwr |>
    kwr_clean_queries() |>
    dplyr::select(.data$query_normalized, .data$volume) |>
    tidytext::unnest_ngrams(
      output = "token", input = "query_normalized",
      n = max_words, n_min = min_words, drop = FALSE
    ) |>
    dplyr::group_by(.data$token) |>
    dplyr::summarize(volume = sum(.data$volume), n = dplyr::n()) |>
    dplyr::filter(
      .data$volume >= min_volume,
      .data$n >= min_n
    ) |>
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$volume))
}
