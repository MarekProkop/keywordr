#' Imports MarketingMiner CSV file(s) to an empty kwresearch object
#'
#' @param kwr An empty kwresearch object.
#' @param path Either a single character string containing a path to a folder,
#'   or a character vector, where each item is a path to a CSV file.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return The provided kwresearch object with imported queries.
#' @export
#'
#' @examples
#' # Import 2 files:
#' \dontrun{
#' kwr <- kwresearch() |>
#'   kwr_import_mm(c("f1.csv", "f2.csv"))
#' }
#'
#' # Import all CSV files in th data-raw folder
#' \dontrun{
#' kwr <- kwresearch() |>
#'   kwr_import_mm("data-raw")
#' }
kwr_import_mm <- function(kwr, path, quiet = FALSE) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_true(kwr$status == "empty")
  checkmate::assert(
    checkmate::check_file_exists(path, access = "r"),
    checkmate::check_directory_exists(path, access = "r"),
    checkmate::check_character(path)
  )
  checkmate::assert_flag(quiet)
  if (length(path) == 1 && file.info(path)$isdir) {
    path <- list.files(path = path, full.names = TRUE, pattern = "\\.csv$")
  }
  queries <- path |> purrr::map_dfr(import_mm_file, quiet)
  kwr |> kwr_import(queries)
}


# Private functions -------------------------------------------------------


#' Imports a single MarketingMiner CSV file
#'
#' @param path Full path and name of the imported file.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A tibble with columns: query, volume, cpc, input, source. Rows are
#' just copied from the CSV file without deduplication or any other
#' modification.
import_mm_file <- function(path, quiet = FALSE) {
  if (!quiet) {
    message("Importing ", path)
  }
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
    dplyr::filter(!is.na(.data$keyword)) |>
    dplyr::mutate(
      cpc = as.numeric(
        stringr::str_replace(.data$cpc, stringr::fixed(" K\u010D"), "")
      ),
      source = dplyr::recode(.data$source, Sklik = "Seznam", AdWords = "Google")
    ) |>
    dplyr::rename(volume = .data$search_volume, query = .data$keyword) |>
    dplyr::select(
      .data$query, .data$input, .data$source, .data$volume, .data$cpc
    )
}
