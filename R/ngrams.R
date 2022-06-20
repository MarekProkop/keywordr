#' Generates n-grams from queries
#'
#' @param x A \code{\link{kwresearch}} object, which queries will be n-grams
#'   calculated from, or a data frame of queries and volume.
#' @param max_words Maximum number of words in n-grams.
#' @param min_words Minimum number of words in n-grams.
#' @param min_n Minimum number of queries. Only the n-grams with at least this
#'   number of queries will be included.
#' @param min_volume Minimum search volume per n-gram. Only the n-grams with at
#'   least this volume will be included.
#' @param remove_nested If TRUE, n-grams fully contained in another n-gram
#'   (number of queries must be the same) are filtered out from the result. May
#'   be slower.
#'
#' @return A tibble of n-grams with a basic stats (number of queries and sum of
#'   search volumes). The n-grams are ordered descendingly by number of queries
#'   and search volume. Use dplyr::arrange to change order. If stop words are
#'   set with the kwr_use_stopwords, they are removed from unigrams.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research", "seo research"),
#'   volume = c(1000, 500, 100)
#' )
#' kwr <- kwresearch(queries)
#' kwr_ngrams(kwr)
kwr_ngrams <- function(
  x,
  max_words = 4, min_words = 1, min_n = 1, min_volume = 0,
  remove_nested = TRUE
) {
  checkmate::assert(
    checkmate::check_class(x, "kwresearch"),
    checkmate::check_class(x, "data.frame")
  )
  checkmate::assert_int(min_words, lower = 1, upper = 10)
  checkmate::assert_int(max_words, lower = min_words, upper = 10)
  checkmate::assert_count(min_n)
  checkmate::assert_count(min_volume)
  checkmate::assert_flag(remove_nested)

  if (inherits(x, "kwresearch")) {
    checkmate::assert_choice(x$status, c("data", "pruned", "classified"))
    df <- x |> kwr_queries()
  } else {
    df <- x
  }
  ngrams <- df |>
    dplyr::select(.data$query_normalized, .data$volume) |>
    tidytext::unnest_ngrams(
      output = "token", input = "query_normalized",
      n = max_words, n_min = min_words, drop = FALSE
    )
  if (inherits(x, "kwresearch") && !is.null(x$stopwords)) {
    ngrams <- ngrams |>
      dplyr::filter(!.data$token %in% x$stopwords)
  }
  ngrams <- ngrams |>
    aggregate_ngrams() |>
    dplyr::filter(.data$n >= min_n, .data$volume >= min_volume) |>
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$volume), .data$token)
  if (remove_nested) {
    ngrams <- ngrams |>
      remove_nested_ngrams(max_words)
  }
  ngrams
}

#' Generates n-grams from queries and filter only those that match any existing
#' query
#'
#' @description In other words, it lists queries that are contained in other
#'   queries.
#'
#' @param x A \code{\link{kwresearch}} object, which queries will be n-grams
#'   calculated from, or a data frame of queries and volume.
#' @param max_words Maximum number of words in n-grams.
#' @param min_words Minimum number of words in n-grams.
#' @param min_n Minimum number of queries. Only the n-grams with at least this
#'   number of queries will be included.
#' @param min_volume Minimum search volume per n-gram. Only the n-grams with at
#'   least this volume will be included.
#'
#' @return A tibble of n-grams with a basic stats (number of queries and sum of
#'   search volumes). The n-grams are ordered descendingly by number of queries
#'   and search volume. Use dplyr::arrange to change order.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research", "seo research"),
#'   volume = c(1000, 500, 100)
#' )
#' kwr <- kwresearch(queries)
#' kwr_subqueries(kwr)
kwr_subqueries <- function(
  x, max_words = 5, min_words = 1, min_n = 1, min_volume = 0
) {
  checkmate::assert(
    checkmate::check_class(x, "kwresearch"),
    checkmate::check_class(x, "data.frame")
  )
  checkmate::assert_int(max_words, lower = 1, upper = 10)
  checkmate::assert_count(min_n)
  checkmate::assert_count(min_volume)

  if (inherits(x, "kwresearch")) {
    checkmate::assert_choice(x$status, c("data", "pruned", "classified"))
    df <- x |> kwr_queries()
  } else {
    df <- x
  }
  df |>
    dplyr::select(.data$query_normalized, .data$volume) |>
    tidytext::unnest_ngrams(
      output = "token", input = "query_normalized",
      n = max_words, n_min = min_words, drop = FALSE
    ) |>
    dplyr::filter(
      .data$token != .data$query_normalized,
      .data$token %in% .data$query_normalized
    ) |>
    aggregate_ngrams() |>
    dplyr::filter(
      .data$volume >= min_volume,
      .data$n >= min_n
    ) |>
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$volume))
}

#' Finds collocations, i.e. multiword phrases that are more likely than their
#' single words
#'
#' @param x A \code{\link{kwresearch}} object, which queries will be
#'   collocations calculated from, or a data frame of queries and volume.
#' @param min_volume_prop Minimum proportion.
#' @param min_n Minimum number of queries. Only the n-grams with at least this
#'   number of queries will be included.
#' @param quanteda If TRUE, will use the 'quanteda' package, which is
#'   recommended.
#'
#' @return A tibble of n-grams with a basic stats (number of queries and sum of
#'   search volumes). The n-grams are ordered descendingly by number of queries
#'   and search volume. Use dplyr::arrange to change order.
#' @export
kwr_collocations <- function(
  x, min_volume_prop = 0.5, min_n = 2, quanteda = TRUE
) {
  checkmate::assert(
    checkmate::check_class(x, "kwresearch"),
    checkmate::check_class(x, "data.frame")
  )
  checkmate::assert_double(min_volume_prop, len = 1, lower = 0, upper = 1)
  checkmate::assert_count(min_n)

  if (inherits(x, "kwresearch")) {
    checkmate::assert_choice(x$status, c("data", "pruned", "classified"))
    df <- x |> kwr_queries()
  } else {
    df <- x
  }
  if (quanteda) {
    df |>
      quanteda::corpus(
        docid_field = "query_normalized",
        text_field = "query_normalized"
      ) |>
      quanteda::tokens(remove_punct = TRUE) |>
      quanteda.textstats::textstat_collocations(size = min_n:4) |>
      tibble::as_tibble()
  } else {
    df |>
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
      dplyr::filter(
        .data$volume_prop >= min_volume_prop,
        .data$n >= min_n
      ) |>
      remove_nested_ngrams(4) |>
      dplyr::select(
        .data$token, .data$n, .data$volume, .data$n_prop, .data$volume_prop
      ) |>
      dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$volume))
  }
}


# Private functions -------------------------------------------------------


aggregate_ngrams <- function(ngrams) {
  ngrams |>
    dplyr::group_by(.data$token) |>
    dplyr::summarize(n = dplyr::n(), volume = sum(.data$volume))
}

remove_nested_ngrams <- function(df, n) {
  nested <- df |>
    tidytext::unnest_ngrams(
      output = .data$subtoken,
      input = .data$token,
      n = n, n_min = 1, drop = FALSE
    ) |>
    dplyr::group_by(.data$n) |>
    dplyr::filter(
      .data$subtoken %in% .data$token,
      .data$subtoken != .data$token
    ) |>
    dplyr::ungroup() |>
    dplyr::select(token = .data$subtoken)

  df |> dplyr::anti_join(nested, by = "token")
}
