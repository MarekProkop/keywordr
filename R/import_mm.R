#' Imports Marketing Miner CSV file(s) to an empty kwresearch object
#'
#' @param kwr An empty kwresearch object.
#' @param path Either a single character string containing a path to a folder,
#'   or a character vector, where each item is a path to a CSV file.
#' @param quiet If TRUE prints no messages.
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

  if (!quiet) start_time <- Sys.time()
  if (length(path) == 1 && file.info(path)$isdir) {
    path <- list.files(path = path, full.names = TRUE, pattern = "\\.csv$")
  }
  queries <- path |> purrr::map_dfr(import_mm_file, quiet)
  result <- kwr |> kwr_import(queries)
  if (!quiet) {
    message(stringr::str_glue(
      "Imported {nrow(queries)} rows from {length(path)} file(s), aggregated into {nrow(result$source_data)} queries, normalized into {nrow(result$clean_data)} queries. Duration: {round(Sys.time() - start_time, 3)}s."
    ))
  }
  result
}


# Private functions -------------------------------------------------------


#' Imports a single Marketing Miner CSV file
#'
#' @param path Full path and name of the imported file.
#' @param quiet If TRUE prints no messages.
#'
#' @return A tibble with columns: query, volume, cpc, input, source. Rows are
#' just copied from the CSV file without deduplication or any other
#' modification.
#' @keywords internal
import_mm_file <- function(path, quiet = FALSE) {
  if (!quiet) {
    message("Importing ", path)
  }
  readr::read_csv(path, col_select = 1:5, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(.data$keyword)) |>
    dplyr::mutate(
      source = dplyr::recode(.data$source, Sklik = "Seznam", AdWords = "Google")
    ) |>
    dplyr::select(query = 2, input = 1, source = 5, volume = 4, cpc = 3)
}
