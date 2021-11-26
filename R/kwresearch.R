#' @title Creates an object of the kwresearch class
#'
#' @description An object of the kwresearch class is a home to your keyword research. You can import datasets into it, research queries, clasify them and export the final keyword analysis.
#'
#' @param queries Queries to import as a data frame with at leas two colums: query (string) and volume (numeric). Optionally the data frame can contain three additional columns: cpc (double), input (char) and source (char).
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
kwresearch <- function(queries = NULL) {
  result <- structure(
    list(status = "empty"),
    class = "kwresearch"
  )
  if (!is.null(queries)) {
    empty_df <- tibble::tibble(
      query = character(), volume = integer(), cpc = double(), input = character(), source = character()
    )
    result$sourceData <- queries |>
      dplyr::bind_rows(empty_df) |>
      dplyr::group_by(query, input, source) |>
      dplyr::summarise(
        volume = dplyr::last(volume),
        cpc = dplyr::last(cpc),
        .groups = "drop"
      )
    result$cleanData <- clean_source_data(result$sourceData)
    result$classifiedData <- NULL
    result$status <- "data"
  }
  result
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
  kwr$sourceData
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
  kwr$cleanData
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
  stopifnot(!is.null(kwr$classifiedData))
  kwr$classifiedData
}


# Private functions -------------------------------------------------------


clean_source_data <- function(sourceData) {
  sourceData |>
    dplyr::mutate(
      query_normalized = normalize_queries(accentize_queries(query, volume), volume),
      .after = query
    ) |>
    aggregate_clean_data()
}

accentize_queries <- function(x, vol) {
  tibble::tibble(
    x,
    ascii = stringi::stri_trans_general(x, "Latin-ASCII"),
    dist = vol * ((stringdist::stringdist(x, ascii) + 1) ** 3),
    r = 1:length(x)
  ) |>
    dplyr::arrange(ascii, dplyr::desc(dist), dplyr::desc(vol)) |>
    dplyr::group_by(ascii) |>
    dplyr::mutate(accentized = dplyr::first(x)) |>
    dplyr::arrange(r) |>
    dplyr::pull(accentized)
}

normalize_queries <- function(x, value) {
  tibble::tibble(
    x,
    value,
    sorted = stringr::str_split(x, stringr::boundary("word")) |>
      purrr::map_chr(~ stringr::str_c(stringr::str_sort(.x), collapse = " "))
  ) |>
    dplyr::group_by(sorted) |>
    dplyr::mutate(normalized = x[which.max(value)]) |>
    dplyr::pull(normalized)
}

aggregate_clean_data <- function(mmData) {
  mmData |>
    dplyr::group_by(query_normalized) |>
    dplyr::summarise(
      n_queries = dplyr::n(),
      volume = sum(volume),
      cpc = max(cpc),
      query_original = stringr::str_c(unique(query), collapse = ","),
      input = stringr::str_c(unique(input), collapse = ","),
      source = stringr::str_c(unique(source), collapse = ",")
    ) |>
    dplyr::arrange(dplyr::desc(volume), query_normalized)
}

