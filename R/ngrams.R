#' Generate n-grams from queries
#'
#' @param queries A data frame containing queries and their search volumes.
#'   Should be the result of functions kwr_source_queries, kwr_clean_queries,
#'   kwr_classified_queries, or kwr_valid_queries.
#' @param max_words Maximum number of words in n-grams.
#' @param min_words Minimum number of words in n-grams.
#' @param min_n Minimum number of queries. Only the n-grams with at least this
#'   number of queries will be included.
#' @param min_volume Minimum search volume per n-gram. Only the n-grams with at
#'   least this volume will be included.
#' @param order A column the result will be reverse ordered by.
#'
#' @return A tibble of n-grams with a basic stats (nuber of queries and sum of
#'   search volumes).
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#' kwr |> kwr_clean_queries() |>
#'   kwr_ngrams()
kwr_ngrams <- function(
  queries, max_words = 4, min_words = 1, min_n = 1, min_volume = 1,
  order = volume
) {
  if ("query_normalized" %in% names(queries)) {
    query_col <- "query_normalized"
  } else {
    if ("query" %in% names(queries)) {
      query_col <- "query"
    } else {
      stop("The argument queries must have the first column named
           'query', or 'query_normalized'.")
    }
  }
  queries |>
    dplyr::select(.data[[query_col]], .data$volume) |>
    tidytext::unnest_ngrams(
      output = "token", input = query_col,
      n = max_words, n_min = min_words, drop = FALSE
    ) |>
    dplyr::group_by(.data$token) |>
    dplyr::summarize(volume = sum(.data$volume), n = dplyr::n()) |>
    dplyr::filter(
      .data$volume >= min_volume,
      .data$n >= min_n
    ) |>
    dplyr::arrange(dplyr::desc({{ order }}))
}
