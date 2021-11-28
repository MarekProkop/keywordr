#' Generates n-grams from queries
#'
#' @description N-grams fully contained in another n-gram (number of queries
#'   must be the same) are filter out from the result.
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
  kwr, max_words = 4, min_words = 1, min_n = 1, min_volume = 0
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
    dplyr::arrange(.data$token) |>
    dplyr::filter(
      !(stringr::str_detect(
        dplyr::lead(.data$token, default = ""), stringr::fixed(.data$token)
      ) & .data$n == dplyr::lead(.data$n)),
      .data$volume >= min_volume,
      .data$n >= min_n
    ) |>
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$volume))
}

#' Generates n-grams from queries and filter only those that equals any existing
#' query
#'
#' @description In other words, it lists queries that are contained in other
#'   queries.
#'
#' @param kwr A kwresearch object, which clean queries will be n-grams
#'   calculated from.
#' @param max_words Maximum number of words in n-grams.
#' @param min_n Minimum number of queries. Only the n-grams with at least this
#'   number of queries will be included.
#' @param min_volume Minimum search volume per n-gram. Only the n-grams with at
#'   least this volume will be included.
#'
#' @return A tibble of n-grams with a basic stats (nuber of queries and sum of
#'   search volumes). The n-grams are orderd descendingly by number of queries
#'   and search volume. Use dplyr::arrange to change order.
#' @export
kwr_subqueries <- function(kwr, max_words = 5, min_n = 1, min_volume = 0) {
  kwr |>
    kwr_clean_queries() |>
    dplyr::select(.data$query_normalized, .data$volume) |>
    tidytext::unnest_ngrams(
      output = "token", input = "query_normalized",
      n = max_words, n_min = 2, drop = FALSE
    ) |>
    dplyr::filter(
      .data$token != .data$query_normalized,
      .data$token %in% .data$query_normalized
    ) |>
    dplyr::group_by(.data$token) |>
    dplyr::summarize(volume = sum(.data$volume), n = dplyr::n()) |>
    dplyr::filter(
      .data$volume >= min_volume,
      .data$n >= min_n
    ) |>
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$volume))
}

#' Finds collocations, i.e. multiword phrases that are more likely than their
#' single words
#'
#' @description N-grams fully contained in another n-gram (number of queries
#'   must be the same) are filter out from the result.
#'
#' @param kwr A kwresearch object, which clean queries will be used.
#' @param min_volume_prop Minimum proportion.
#' @param min_n Minimum number of queries. Only the n-grams with at least this
#'   number of queries will be included.
#'
#' @return A tibble of n-grams with a basic stats (nuber of queries and sum of
#'   search volumes). The n-grams are orderd descendingly by number of queries
#'   and search volume. Use dplyr::arrange to change order.
#' @export
kwr_collocations <- function(kwr, min_volume_prop = 0.5, min_n = 2) {
  kwr |>
    kwr_clean_queries() |>
    dplyr::select(.data$query_normalized, .data$volume) |>
    tidytext::unnest_ngrams(
      output = "token", input = "query_normalized", n = 4, n_min = 2
    ) |>
    dplyr::relocate(.data$volume, .after = .data$token) |>
    tidyr::drop_na() |>
    dplyr::add_count(.data$token) |>
    tidytext::unnest_tokens(
      output = "word", input = "token", token = "words", drop = FALSE
    ) |>
    dplyr::group_by(.data$word) |>
    dplyr::mutate(
      volume_word = sum(.data$volume),
      n_word = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      volume_prop = .data$volume / .data$volume_word,
      n_prop = .data$n / .data$n_word
    ) |>
    dplyr::group_by(.data$token) |>
    dplyr::summarise(
      volume = dplyr::first(.data$volume),
      n = dplyr::first(.data$n),
      volume_prop = max(.data$volume_prop),
      n_prop = max(.data$n_prop)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$token) |>
    dplyr::filter(
      !(stringr::str_detect(
        dplyr::lead(.data$token, default = ""), stringr::fixed(.data$token)
      ) & .data$n == dplyr::lead(.data$n)),
      .data$volume_prop >= min_volume_prop,
      .data$n >= min_n
    ) |>
    dplyr::select(
      .data$token, .data$volume, .data$n, .data$volume_prop, .data$n_prop
    ) |>
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$volume))
}
