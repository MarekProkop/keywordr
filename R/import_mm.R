#' Imports MarketingMiner CSV file(s)
#'
#' @param path Either a single character string containing a path to a folder, or a character vector, where each item is a path to a CSV file.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A tibble in a form suitable for creating a kwresearch object.
#' @export
#'
#' @examples
#' # Import 2 files:
#' \dontrun{
#' queries <- kwr_import_mm(c("f1.csv", "f2.csv"))
#' }
#'
#' # Import all CSV files in th data-raw folder
#' # and create a kwresearch object with them
#' \dontrun{
#' kwr <- kwr_import_mm("data-raw") |>
#'   kwresearch()
#' }
kwr_import_mm <- function(path, quiet = FALSE) {
  if (length(path) == 1 && file.info(path)$isdir) {
    path <- list.files(path = path, full.names = TRUE, pattern = "\\.csv$")
  }
  path |> purrr::map_dfr(import_mm_file, quiet)
}

#' Imports a single MarketingMiner CSV file
#'
#' @param path Full path and name of the imported file.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A tibble with columns: query, volume, cpc, input, source. Rows are just copied from the CSV file without deduplication or any other modification.
import_mm_file <- function(path, quiet = FALSE) {
  if (!quiet) { message("Importing ", path) }
  readr::read_csv(path, col_types = readr::cols(
    Input = readr::col_character(),
    Keyword = readr::col_character(),
    CPC = readr::col_character(),
    `Search Volume` = readr::col_double(),
    Source = readr::col_character(),
    `Monthly January` = readr::col_double(),
    `Monthly February` = readr::col_double(),
    `Monthly March` = readr::col_double(),
    `Monthly April` = readr::col_double(),
    `Monthly May` = readr::col_double(),
    `Monthly June` = readr::col_double(),
    `Monthly July` = readr::col_double(),
    `Monthly August` = readr::col_double(),
    `Monthly September` = readr::col_double(),
    `Monthly October` = readr::col_double(),
    `Monthly November` = readr::col_double(),
    `Monthly December` = readr::col_double()
  )) |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(keyword)) |>
    dplyr::mutate(
      cpc = as.numeric(stringr::str_replace(cpc, stringr::fixed(" K\u010D"), "")),
      source = dplyr::recode(source, Sklik = "Seznam", AdWords = "Google")
    ) |>
    dplyr::rename(volume = search_volume, query = keyword) |>
    dplyr::select(query, input, source, volume, cpc)
}
