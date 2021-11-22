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
    #result$cleanData <- .kwr_cleanMM_(result$sourceData)
    result$status <- "data"
  }
  result
}
