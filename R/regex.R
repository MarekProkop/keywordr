#' Creates regular expression from a single string
#'
#' The function does the following two things:
#'
#' 1. Accented letters in a string are replaced with a character class
#' containing both the accented letter and the same letter without accent.
#'
#' 2. If `and` argument is provided, the reulting regex will match both strings
#' in any order.
#'
#' @param x Input string.
#' @param and Optional string that should be matched as well.
#'
#' @return Regular expression.
#' @export
#'
#' @examples
#'
#' kwr_build_regex("vajíčko")
#'
#' # "vaj[íi][čc]ko"
#'
#' kwr_build_regex(x = "abc", and = "xyz")
#'
#' # "xyz.+abc|abc.+xyz"
kwr_build_regex <- function(x, and = NULL) {
  x <- replace_accents(x)
  if (is.null(and)) {
    return(x)
  } else {
    and <- replace_accents(and)
    return(paste0(and, ".+", x, "|", x, ".+", and))
  }
}


#' Explore a regex pattern applied on queries
#'
#' @param kwr A kwresearch object.
#' @param pattern A single regular experession.
#' @param and A vector of regular expressions. If more than one, they are
#'   treated as OR.
#' @param except A vector of regular expressions. If more than one, they are
#'   treated as OR.
#' @param stopwords A vector of stopwords to remove from n-grams.
#'
#' @return A list of data frames.
#' @export
kwr_test_regex <- function(kwr, pattern, and = NULL, except = NULL, stopwords = kwr_stopwords()) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("data", "pruned", "classified"))
  checkmate::assert_string(pattern)

  full <- kwr |>
    kwr_queries() |>
    dplyr::select(.data$query_normalized) |>
    dplyr::filter(stringr::str_detect(.data$query_normalized, pattern))
  if (!is.null(except)) {
    full <- full |>
      dplyr::filter(
        !stringr::str_detect(.data$query_normalized, join_patterns(except))
      )
  }
  if (!is.null(and)) {
    full <- full |>
      dplyr::filter(
        stringr::str_detect(.data$query_normalized, join_patterns(and))
      )
  }
  full <- full |>
    dplyr::mutate(
      match = stringr::str_extract(.data$query_normalized, pattern),
      word = stringr::str_extract(
        .data$query_normalized, stringr::str_c("\\w*", pattern, "\\w*")
      ),
      schema = stringr::str_replace(
        .data$query_normalized,
        stringr::str_c("\\w*", pattern, "\\w*"), "*"
      ),
      pred = stringr::str_replace(
        stringr::str_replace(.data$schema, " \\*.*", ""), ".*\\*.*", ""
      ),
      succ = stringr::str_replace(
        stringr::str_replace(.data$schema, ".*\\* ", ""), ".*\\*.*", ""
      )
    ) |>
    dplyr::select(
      .data$query_normalized, .data$pred, .data$word, .data$succ, .data$match
    )
  around <- full |>
    tidyr::pivot_longer(
      cols = c(.data$pred, .data$succ), values_to = "around"
    ) |>
    dplyr::filter(.data$around != "") |>
    dplyr::count(.data$around, sort = TRUE)
  around_ngrams <- around |>
    tidytext::unnest_ngrams("token", "around", n = 4, n_min = 1) |>
    dplyr::count(.data$token, sort = TRUE) |>
    remove_nested_ngrams(4)
  if (!is.null(stopwords)) {
    around_ngrams <- around_ngrams |> kwr_remove_stopwords(stopwords)
  }
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


# Private functions -------------------------------------------------------


replace_accents <- function(x) {
  char_vec <- stringr::str_split(x, "")[[1]]
  char_vec |> purrr::map_chr(function(x) {
    ascii = stringi::stri_trans_general(x, "Latin-ASCII")
    if (ascii != x) {
      stringr::str_c("[", x, ascii, "]")
    } else {
      x
    }
  }) |>
    stringr::str_c(collapse = "")
}
