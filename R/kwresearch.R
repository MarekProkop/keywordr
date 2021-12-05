#' @title Creates an object of the kwresearch class
#'
#' @description An object of the kwresearch class is a home to your keyword
#'   research. You can import datasets into it, research queries, clasify them
#'   and export the final keyword analysis.
#'
#' @param queries Queries to import as a data frame with at least two colums:
#'   query (string) and volume (numeric). Optionally the data frame can contain
#'   three additional columns: cpc (double), input (char) and source (char). If
#'   you don't provide this argument, you can import queries later with
#'   kwr_import or kwr_import_mm.
#' @param accentize The import functions tries to add correct accents (diakritic
#'   marks) to queries without them.
#' @param normalize The import funkctions tries to unite queries, which differ
#'   only by order of words.
#'
#' @return An object of the kwresearch class.
#' @export
#'
#' @examples
#' kwr <- kwresearch()
#'
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
kwresearch <- function(queries = NULL, accentize = TRUE, normalize = TRUE) {
  checkmate::assert_flag(accentize)
  checkmate::assert_flag(normalize)
  result <- structure(
    list(
      status = "empty",
      options = list(
        accentize = accentize,
        normalize = normalize
      )
    ),
    class = "kwresearch"
  )
  if (!is.null(queries)) {
    result <- result |>
      kwr_import(queries)
  }
  result
}

#' Imports queries to a kwresearch object
#'
#' @param kwr An empty kwresearch object.
#' @param queries A data frame with at least one column query and possibly with
#'   additional colums volume, cpc, imput and source.
#'
#' @return The provided kwresearch object with imported queries.
#' @export
kwr_import <- function(kwr, queries) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_string(kwr$status, pattern = "^empty$")
  checkmate::assert_data_frame(queries)
  empty_df <- tibble::tibble(
    query = character(),
    volume = integer(),
    cpc = double(),
    input = character(),
    source = character()
  )
  kwr$source_data <- queries |>
    dplyr::bind_rows(empty_df) |>
    dplyr::group_by(.data$query, .data$input, .data$source) |>
    dplyr::summarise(
      volume = dplyr::last(.data$volume),
      cpc = dplyr::last(.data$cpc),
      .groups = "drop"
    )
  kwr$clean_data <- clean_source_data(kwr$source_data)
  kwr$status <- "data"
  kwr
}


#' Outputs raw, source queries
#'
#' @param kwr A kwresearch object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#' kwr |> kwr_source_queries()
kwr_source_queries <- function(kwr) {
  kwr$source_data
}

#' Outputs cleaned (normalized and accentized) queries
#'
#' @param kwr A kwresearch object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#' kwr |> kwr_clean_queries()
kwr_clean_queries <- function(kwr) {
  kwr$clean_data
}

#' Outputs classified queries
#'
#' @param kwr A kwresearch object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#' \dontrun{
#' kwr |>
#'   kwr_classify("C:/Dev/R/Public/keyworder/tests/test-data/recipes.yml") |>
#'   kwr_classified_queries()
#' }
kwr_classified_queries <- function(kwr) {
  stopifnot(!is.null(kwr$classified_data))
  kwr$classified_data
}

#' Set a stopword list to use with n-gram functions
#'
#' @param kwr A kwresearch object.
#' @param stopwords Stopwords as a character vector. You can use the
#'   kwr_stopwords() function. These words will be removed from unigram
#'   listings.
#'
#' @return A kwresearch object with stopwords set.
#' @export
kwr_use_stopwords <- function(kwr, stopwords) {
  kwr$stopwords <- stopwords
  kwr
}

# Private functions -------------------------------------------------------


clean_source_data <- function(source_data) {
  source_data |>
    dplyr::mutate(
      query_normalized = normalize_queries(
        accentize_queries(.data$query, .data$volume), .data$volume
      ),
      .after = .data$query
    ) |>
    aggregate_clean_data()
}

#' Tries to add accents (´ˇ¨ etc.) where they should be
#'
#' @param x Character vector of queries.
#' @param vol Character vector of search volumes corresponding to queries in x.
#'
#' @return Character vector of all queries in x, with accents added to some
#' of them.
#' @importFrom rlang .data
accentize_queries <- function(x, vol) {
  tibble::tibble(
    x,
    ascii = stringi::stri_trans_general(x, "Latin-ASCII"),
    dist = vol * ((stringdist::stringdist(x, .data$ascii) + 1) ** 3),
    r = seq_len(length(x))
  ) |>
    dplyr::arrange(.data$ascii, dplyr::desc(.data$dist), dplyr::desc(vol)) |>
    dplyr::group_by(.data$ascii) |>
    dplyr::mutate(accentized = dplyr::first(x)) |>
    dplyr::arrange(.data$r) |>
    dplyr::pull(.data$accentized)
}

normalize_queries <- function(x, value) {
  tibble::tibble(
    x,
    value,
    sorted = stringr::str_split(x, stringr::boundary("word")) |>
      purrr::map_chr(~ stringr::str_c(stringr::str_sort(.x), collapse = " "))
  ) |>
    dplyr::group_by(.data$sorted) |>
    dplyr::mutate(normalized = x[which.max(.data$value)]) |>
    dplyr::pull(.data$normalized)
}

aggregate_clean_data <- function(mm_data) {
  mm_data |>
    dplyr::group_by(.data$query_normalized) |>
    dplyr::summarise(
      n_queries = dplyr::n(),
      volume = sum(.data$volume),
      cpc = max(.data$cpc),
      query_original = stringr::str_c(unique(.data$query), collapse = ","),
      input = stringr::str_c(unique(.data$input), collapse = ","),
      source = stringr::str_c(unique(.data$source), collapse = ",")
    ) |>
    dplyr::arrange(dplyr::desc(.data$volume), .data$query_normalized)
}
